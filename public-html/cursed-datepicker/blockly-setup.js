/**
 * Blockly Setup for Countdown Datepicker
 * Defines custom blocks and workspace configuration
 */

class BlocklyManager {
    constructor(containerId, gameLogic) {
        this.containerId = containerId;
        this.gameLogic = gameLogic;
        this.workspace = null;
        this.solutionBlock = null;
        this.numberBlocks = [];
        this.numberBlockPositions = new Map(); // Store original positions
        this.skipCustomDispose = false; // Flag to skip custom dispose handler
    }

    /**
     * Initialize Blockly workspace with custom blocks
     */
    init() {
        this.defineCustomBlocks();
        this.createWorkspace();
        this.populateWorkspace();
        this.setupEventListeners();
    }

    /**
     * Define custom block types
     */
    defineCustomBlocks() {
        // Solution block (year/month/day structure)
        Blockly.Blocks['date_solution'] = {
            init: function() {
                this.appendValueInput('YEAR')
                    .setCheck('Number')
                    .appendField('Year:');
                this.appendValueInput('MONTH')
                    .setCheck('Number')
                    .appendField('Month:');
                this.appendValueInput('DAY')
                    .setCheck('Number')
                    .appendField('Day:');
                this.setColour(230);
                this.setTooltip('Construct your date of birth');
                this.setDeletable(false);
                this.setMovable(false);
            }
        };

        // Number block (non-editable)
        Blockly.Blocks['number_value'] = {
            init: function() {
                this.appendDummyInput()
                    .appendField(new Blockly.FieldLabelSerializable('0'), 'NUM');
                this.setOutput(true, 'Number');
                this.setColour(160);
                this.setDeletable(false);
            }
        };

        // Arithmetic blocks with result display - external inputs (tree structure)
        const operations = [
            { op: 'add', symbol: '+', color: 210 },
            { op: 'subtract', symbol: '-', color: 260 },
            { op: 'multiply', symbol: '×', color: 310 },
            { op: 'divide', symbol: '÷', color: 0 }
        ];

        operations.forEach(({ op, symbol, color }) => {
            Blockly.Blocks[`math_${op}_result`] = {
                init: function() {
                    // External inputs with result at top
                    this.appendValueInput('A')
                        .setCheck('Number')
                        .appendField(new Blockly.FieldLabel('?'), 'RESULT');
                    this.appendValueInput('B')
                        .setCheck('Number')
                        .appendField(symbol);
                    this.setInputsInline(false);
                    this.setOutput(true, 'Number');
                    this.setColour(color);
                    this.setTooltip(`${op} two numbers`);
                }
            };
        });
    }

    /**
     * Create the Blockly workspace
     */
    createWorkspace() {
        this.workspace = Blockly.inject(this.containerId, {
            toolbox: this.createToolbox(),
            trashcan: false,
            scrollbars: true,
            zoom: {
                controls: true,
                wheel: true,
                startScale: 1.0,
                maxScale: 2,
                minScale: 0.5,
                scaleSpeed: 1.1
            },
            grid: {
                spacing: 20,
                length: 3,
                colour: '#ccc',
                snap: true
            },
            renderer: 'geras'
        });
    }

    /**
     * Create the toolbox with only arithmetic operations
     */
    createToolbox() {
        return `
        <xml>
            <block type="math_add_result"></block>
            <block type="math_subtract_result"></block>
            <block type="math_multiply_result"></block>
            <block type="math_divide_result"></block>
        </xml>
        `;
    }

    /**
     * Populate workspace with fixed blocks
     */
    populateWorkspace() {
        const numbers = this.gameLogic.getNumbers();
        const grid = this.workspace.getGrid();
        const gridSize = 20;
        const numbersStartX = 10 + gridSize * 4;
        const numbersStartY = 10;
        const numberSpacing = gridSize * 3; // 60px spacing (3 grid units)

        // Create solution block first (below where numbers will be)
        this.solutionBlock = this.workspace.newBlock('date_solution');
        this.solutionBlock.setDeletable(false);
        this.solutionBlock.setMovable(true);
        this.solutionBlock.initSvg();
        this.solutionBlock.render();
        const solutionYOffset = gridSize * 3;
        const solutionCoord = grid.alignXY(new Blockly.utils.Coordinate(gridSize, solutionYOffset));
        this.solutionBlock.moveTo(solutionCoord);

        // Create number blocks in one row at top
        numbers.forEach((num, index) => {
            const block = this.workspace.newBlock('number_value');
            block.setFieldValue(num, 'NUM');
            block.setDeletable(false);
            block.setMovable(true);
            block.initSvg();
            block.render();

            // Calculate desired position and align to grid
            const desiredX = numbersStartX + index * numberSpacing;
            const desiredY = numbersStartY;
            const alignedCoord = grid.alignXY(new Blockly.utils.Coordinate(desiredX, desiredY));
            block.moveTo(alignedCoord);

            // Store original position
            this.numberBlockPositions.set(block.id, { x: alignedCoord.x, y: alignedCoord.y });
            this.numberBlocks.push(block);
        });
    }

    /**
     * Setup event listeners for block changes
     */
    setupEventListeners() {
        // Update partial results when blocks change
        this.workspace.addChangeListener((event) => {
            if (event.type === Blockly.Events.BLOCK_CHANGE ||
                event.type === Blockly.Events.BLOCK_MOVE) {
                this.updatePartialResults();
            }

            // Handle custom deletion - reset number blocks when operation is deleted
            if (event.type === Blockly.Events.BLOCK_DELETE) {
                if (event.oldJson && event.oldJson.type.startsWith('math_') && event.oldJson.type.endsWith('_result')) {
                    // An operation block was deleted, extract and reset any number blocks in the tree
                    this.resetNumberBlocksFromJson(event.oldJson);
                }
            }
        });

        // Additional protection: Override the isDeletable method for all blocks
        const originalIsDeletable = Blockly.Block.prototype.isDeletable;
        Blockly.Block.prototype.isDeletable = function() {
            if (this.type === 'date_solution' || this.type === 'number_value') {
                return false;
            }
            return originalIsDeletable.call(this);
        };

        // Prevent duplication of all blocks
        const originalIsDuplicatable = Blockly.Block.prototype.isDuplicatable;
        Blockly.Block.prototype.isDuplicatable = function() {
            // Disable duplication for all blocks
            return false;
        };

        // Override dispose for operation blocks to extract number blocks first
        const manager = this;
        const originalDispose = Blockly.Block.prototype.dispose;
        Blockly.Block.prototype.dispose = function() {
            if (this.type.startsWith('math_') && this.type.endsWith('_result') && !manager.skipCustomDispose) {
                // Extract and reset all number blocks before disposing
                manager.extractAndResetNumberBlocks(this);
            }
            originalDispose.call(this);
        };
    }

    /**
     * Extract and reset number blocks from an operation block tree
     */
    extractAndResetNumberBlocks(block) {
        if (!block) return;

        const inputNames = ['A', 'B'];
        inputNames.forEach(inputName => {
            const childBlock = block.getInputTargetBlock(inputName);
            if (childBlock) {
                if (childBlock.type === 'number_value') {
                    // Disconnect and reset this number block
                    const connection = childBlock.outputConnection;
                    if (connection && connection.targetConnection) {
                        connection.disconnect();
                    }

                    // Reset to original position
                    const pos = this.numberBlockPositions.get(childBlock.id);
                    if (pos) {
                        const currentPos = childBlock.getRelativeToSurfaceXY();
                        childBlock.moveBy(pos.x - currentPos.x, pos.y - currentPos.y);
                    }
                } else if (childBlock.type.startsWith('math_') && childBlock.type.endsWith('_result')) {
                    // Recursively process child operation blocks
                    this.extractAndResetNumberBlocks(childBlock);
                }
            }
        });
    }

    /**
     * Reset number blocks from deleted block JSON (backup method)
     */
    resetNumberBlocksFromJson(blockJson) {
        // This is a backup in case the dispose override doesn't catch it
        // We can't easily recreate blocks here, but we can log if needed
        console.log('Operation block deleted:', blockJson.type);
    }

    /**
     * Update partial results in arithmetic blocks
     */
    updatePartialResults() {
        const blocks = this.workspace.getAllBlocks();

        blocks.forEach(block => {
            if (block.type.startsWith('math_') && block.type.endsWith('_result')) {
                try {
                    const aBlock = block.getInputTargetBlock('A');
                    const bBlock = block.getInputTargetBlock('B');

                    if (aBlock && bBlock) {
                        const aValue = this.getBlockValue(aBlock);
                        const bValue = this.getBlockValue(bBlock);

                        if (aValue !== null && bValue !== null) {
                            let result = null;
                            const op = block.type.replace('math_', '').replace('_result', '');

                            switch (op) {
                                case 'add':
                                    result = aValue + bValue;
                                    break;
                                case 'subtract':
                                    result = aValue - bValue;
                                    break;
                                case 'multiply':
                                    result = aValue * bValue;
                                    break;
                                case 'divide':
                                    if (bValue !== 0 && aValue % bValue === 0) {
                                        result = aValue / bValue;
                                    } else {
                                        result = '✗';
                                    }
                                    break;
                            }

                            block.setFieldValue(result !== null ? String(result) : '?', 'RESULT');
                        } else {
                            block.setFieldValue('?', 'RESULT');
                        }
                    } else {
                        block.setFieldValue('?', 'RESULT');
                    }
                } catch (error) {
                    block.setFieldValue('?', 'RESULT');
                }
            }
        });
    }

    /**
     * Get the numeric value from a block (recursive for nested blocks)
     */
    getBlockValue(block) {
        if (!block) return null;

        if (block.type === 'number_value') {
            return parseInt(block.getFieldValue('NUM'));
        }

        if (block.type.startsWith('math_') && block.type.endsWith('_result')) {
            const aBlock = block.getInputTargetBlock('A');
            const bBlock = block.getInputTargetBlock('B');

            const aValue = this.getBlockValue(aBlock);
            const bValue = this.getBlockValue(bBlock);

            if (aValue === null || bValue === null) return null;

            const op = block.type.replace('math_', '').replace('_result', '');

            switch (op) {
                case 'add':
                    return aValue + bValue;
                case 'subtract':
                    return aValue - bValue;
                case 'multiply':
                    return aValue * bValue;
                case 'divide':
                    if (bValue !== 0 && aValue % bValue === 0) {
                        return aValue / bValue;
                    }
                    return null;
            }
        }

        return null;
    }

    /**
     * Get the current date expressions from the solution block
     */
    getDateExpressions() {
        const yearBlock = this.solutionBlock.getInputTargetBlock('YEAR');
        const monthBlock = this.solutionBlock.getInputTargetBlock('MONTH');
        const dayBlock = this.solutionBlock.getInputTargetBlock('DAY');

        return {
            year: yearBlock ? this.blockToExpression(yearBlock) : '',
            month: monthBlock ? this.blockToExpression(monthBlock) : '',
            day: dayBlock ? this.blockToExpression(dayBlock) : ''
        };
    }

    /**
     * Convert a block (and its children) to a mathematical expression string
     */
    blockToExpression(block) {
        if (!block) return '';

        if (block.type === 'number_value') {
            return String(block.getFieldValue('NUM'));
        }

        if (block.type.startsWith('math_') && block.type.endsWith('_result')) {
            const aBlock = block.getInputTargetBlock('A');
            const bBlock = block.getInputTargetBlock('B');

            const aExpr = this.blockToExpression(aBlock);
            const bExpr = this.blockToExpression(bBlock);

            if (!aExpr || !bExpr) return '';

            const op = block.type.replace('math_', '').replace('_result', '');
            const opMap = { add: '+', subtract: '-', multiply: '*', divide: '/' };

            return `(${aExpr} ${opMap[op]} ${bExpr})`;
        }

        return '';
    }

    /**
     * Clear all blocks and recreate solution block and number blocks
     */
    clearAndRecreateBlocks() {
        if (!this.workspace) {
            console.error('Workspace not initialized');
            return;
        }

        // Disable events and custom dispose handler during clear
        this.skipCustomDispose = true;
        Blockly.Events.disable();

        try {
            // Clear entire workspace using Blockly's method
            this.workspace.clear();

            // Clear references
            this.numberBlocks = [];
            this.numberBlockPositions.clear();

            // Recreate blocks using the same logic as initial population
            this.populateWorkspace();
        } finally {
            this.skipCustomDispose = false;
            Blockly.Events.enable();
        }
    }

    /**
     * Build expressions and connect to solution block
     * @param {Object} expressions - Object with yearExpr, monthExpr, dayExpr strings
     */
    buildExpressions(expressions) {
        if (!expressions) return;

        // Step 1: Move all number blocks way down to avoid collisions
        const tempYOffset = 1000; // Move 1000px down
        this.numberBlocks.forEach(block => {
            const currentPos = block.getRelativeToSurfaceXY();
            block.moveBy(0, tempYOffset);
        });

        // Step 2: Parse and build each expression (uses some number blocks)
        const yearBlock = expressions.yearExpr ? this.parseExpression(expressions.yearExpr) : null;
        const monthBlock = expressions.monthExpr ? this.parseExpression(expressions.monthExpr) : null;
        const dayBlock = expressions.dayExpr ? this.parseExpression(expressions.dayExpr) : null;

        // Step 3: Connect to solution block
        if (yearBlock && this.solutionBlock.getInput('YEAR')) {
            this.solutionBlock.getInput('YEAR').connection.connect(yearBlock.outputConnection);
        }
        if (monthBlock && this.solutionBlock.getInput('MONTH')) {
            this.solutionBlock.getInput('MONTH').connection.connect(monthBlock.outputConnection);
        }
        if (dayBlock && this.solutionBlock.getInput('DAY')) {
            this.solutionBlock.getInput('DAY').connection.connect(dayBlock.outputConnection);
        }

        // Step 4: Move any unused number blocks back to original positions
        this.numberBlocks.forEach(block => {
            // If block is not connected (unused), move it back to original position
            if (!block.outputConnection.targetConnection) {
                const originalPos = this.numberBlockPositions.get(block.id);
                if (originalPos) {
                    const currentPos = block.getRelativeToSurfaceXY();
                    block.moveBy(originalPos.x - currentPos.x, originalPos.y - currentPos.y);
                }
            }
        });
    }

    /**
     * Parse an expression string and build corresponding blocks
     */
    parseExpression(expr) {
        expr = expr.trim();

        // Remove all outer matching parentheses
        while (expr.startsWith('(') && expr.endsWith(')')) {
            let depth = 0;
            let isMatching = true;
            for (let i = 0; i < expr.length; i++) {
                if (expr[i] === '(') depth++;
                else if (expr[i] === ')') depth--;

                if (depth === 0 && i < expr.length - 1) {
                    isMatching = false;
                    break;
                }
            }

            if (isMatching) {
                expr = expr.substring(1, expr.length - 1).trim();
            } else {
                break;
            }
        }

        // Try to find the main operator (rightmost, outside parentheses)
        let depth = 0;
        let operatorPos = -1;
        let operator = null;

        for (let i = expr.length - 1; i >= 0; i--) {
            const char = expr[i];
            if (char === ')') depth++;
            else if (char === '(') depth--;
            else if (depth === 0 && ['+', '-', '*', '/'].includes(char)) {
                operatorPos = i;
                operator = char;
                break;
            }
        }

        // If no operator found, it's just a number
        if (operatorPos === -1) {
            const num = parseInt(expr);
            if (isNaN(num)) return null;

            // Find matching number block
            for (const block of this.numberBlocks) {
                const blockValue = parseInt(block.getFieldValue('NUM'));
                if (blockValue === num && !block.outputConnection.targetConnection) {
                    return block;
                }
            }
            return null;
        }

        // Split into left and right expressions
        const leftExpr = expr.substring(0, operatorPos).trim();
        const rightExpr = expr.substring(operatorPos + 1).trim();

        // Create operation block
        const opMap = { '+': 'add', '-': 'subtract', '*': 'multiply', '/': 'divide' };
        const opType = opMap[operator];
        const opBlock = this.workspace.newBlock(`math_${opType}_result`);
        opBlock.initSvg();
        opBlock.render();

        // Recursively build left and right
        const leftBlock = this.parseExpression(leftExpr);
        const rightBlock = this.parseExpression(rightExpr);

        // Connect children
        if (leftBlock) {
            opBlock.getInput('A').connection.connect(leftBlock.outputConnection);
        }
        if (rightBlock) {
            opBlock.getInput('B').connection.connect(rightBlock.outputConnection);
        }

        return opBlock;
    }

    /**
     * Destroy the workspace
     */
    destroy() {
        if (this.workspace) {
            this.workspace.dispose();
            this.workspace = null;
        }
    }
}

// Export for use in main script
window.BlocklyManager = BlocklyManager;
