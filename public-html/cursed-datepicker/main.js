/**
 * Main Application Logic
 * Orchestrates all components and handles state transitions
 */

class CountdownDatepicker {
    constructor() {
        this.gameLogic = new GameLogic();
        this.blocklyManager = null;
        this.controlsPanel = null;
        this.currentScreen = 'age-verification';
        this.lastValidation = null;
        this.lastExpressions = null;
    }

    /**
     * Initialize the application
     */
    async init() {
        try {
            // Load default solution set
            const success = await this.gameLogic.loadSolutions('solutions/no_1_2_no_50.csv');

            if (!success) {
                console.error('Failed to load initial solutions');
                alert('Failed to load puzzle data. Please refresh the page.');
                return;
            }

            // Setup initial screen
            this.setupAgeVerificationScreen();

            // Wait for user to click datepicker
        } catch (error) {
            console.error('Initialization error:', error);
            alert('Failed to initialize. Please refresh the page.');
        }
    }

    /**
     * Setup age verification screen
     */
    setupAgeVerificationScreen() {
        console.log('Setting up age verification screen');

        const dobInput = document.getElementById('dob-input');
        const continueButton = document.getElementById('continue-button');
        const puzzleArea = document.getElementById('puzzle-area');

        console.log('dob-input element:', dobInput);

        if (!dobInput) {
            console.error('Could not find dob-input element!');
            return;
        }

        // Click on datepicker input expands the puzzle
        dobInput.addEventListener('click', () => {
            console.log('Datepicker clicked! Expanding puzzle...');
            if (puzzleArea.classList.contains('collapsed')) {
                this.expandPuzzle();
            }
        });

        // Continue button handler
        continueButton.addEventListener('click', (e) => {
            e.preventDefault();
            if (!continueButton.disabled) {
                this.celebrate();
            }
        });

        console.log('Age verification screen setup complete');
    }

    /**
     * Expand the puzzle area
     */
    expandPuzzle() {
        console.log('expandPuzzle called');

        try {
            const puzzleArea = document.getElementById('puzzle-area');
            const controlsPanel = document.getElementById('controls-panel');
            const container = document.querySelector('.corporate-container');
            const dobInput = document.getElementById('dob-input');

            // Initialize Blockly (only once)
            if (!this.blocklyManager) {
                // Disable transitions temporarily
                container.style.transition = 'none';

                // Expand to full size instantly (no animation)
                container.classList.add('expanded');
                puzzleArea.classList.remove('collapsed');
                puzzleArea.classList.add('expanded');

                // Force reflow to ensure DOM has updated
                container.offsetHeight;

                console.log('Initializing Blockly workspace');
                this.blocklyManager = new BlocklyManager('blockly-workspace', this.gameLogic);
                this.blocklyManager.init();

                // Setup live update listener
                this.blocklyManager.workspace.addChangeListener(() => {
                    this.updateDateInput();
                });

                // Trigger initial update to show ????-??-??
                this.updateDateInput();

                // Now collapse back to narrow (still no animation)
                container.classList.remove('expanded');
                puzzleArea.classList.remove('expanded');
                puzzleArea.classList.add('collapsed');

                // Force reflow
                container.offsetHeight;

                // Re-enable transitions and animate expansion
                container.style.transition = '';

                requestAnimationFrame(() => {
                    puzzleArea.classList.remove('collapsed');
                    puzzleArea.classList.add('expanded');
                    container.classList.add('expanded');
                    dobInput.classList.add('expanded');
                    controlsPanel.classList.add('visible');
                });
            } else {
                // Already initialized, just animate
                puzzleArea.classList.remove('collapsed');
                puzzleArea.classList.add('expanded');
                controlsPanel.classList.add('visible');
                container.classList.add('expanded');
                dobInput.classList.add('expanded');
            }

            // Initialize controls panel (only once)
            if (!this.controlsPanel) {
                console.log('Initializing controls panel');
                this.controlsPanel = new ControlsPanel(
                    this.gameLogic,
                    this.blocklyManager,
                    () => {
                        // Difficulty change callback
                        this.reloadWorkspace();
                    },
                    () => {
                        // Reset callback - update the date input
                        this.updateDateInput();
                    }
                );
                this.controlsPanel.init();
                this.controlsPanel.updateHelpText(); // Set initial help text
            }

            console.log('Puzzle expansion complete');
        } catch (error) {
            console.error('Error in expandPuzzle:', error);
            alert('Failed to load puzzle: ' + error.message);
        }
    }

    /**
     * Format a number for date display (with zero padding and negative handling)
     */
    formatDatePart(value, length) {
        if (value < 0) {
            const abs = Math.abs(value);
            const padded = String(abs).padStart(length, '0');
            return `(-${padded})`;
        }
        return String(value).padStart(length, '0');
    }

    /**
     * Update the date input field with current expressions
     */
    updateDateInput() {
        const dobInput = document.getElementById('dob-input');
        const validationIndicator = document.getElementById('validation-indicator');
        const continueButton = document.getElementById('continue-button');

        const expressions = this.blocklyManager.getDateExpressions();

        // Evaluate each part (or use ? if missing)
        const yearExpr = expressions.year || '';
        const monthExpr = expressions.month || '';
        const dayExpr = expressions.day || '';

        const yearResult = yearExpr ? this.gameLogic.evaluateExpression(yearExpr) : { success: false };
        const monthResult = monthExpr ? this.gameLogic.evaluateExpression(monthExpr) : { success: false };
        const dayResult = dayExpr ? this.gameLogic.evaluateExpression(dayExpr) : { success: false };

        // Build display string (always show something)
        const yearDisplay = yearResult.success ? this.formatDatePart(yearResult.value, 4) : '????';
        const monthDisplay = monthResult.success ? this.formatDatePart(monthResult.value, 2) : '??';
        const dayDisplay = dayResult.success ? this.formatDatePart(dayResult.value, 2) : '??';

        const dateText = `${yearDisplay}-${monthDisplay}-${dayDisplay}`;
        dobInput.value = dateText;

        // Validate if we have all three expressions
        if (yearExpr && monthExpr && dayExpr) {
            const validation = this.gameLogic.validateDate(yearExpr, monthExpr, dayExpr);

            if (validation.valid) {
                // Valid date - green border, no message
                dobInput.className = 'datepicker-input valid';
                validationIndicator.textContent = '';
                validationIndicator.className = 'validation-indicator';
                continueButton.disabled = false;

                // Store for later
                this.lastValidation = validation;
                this.lastExpressions = expressions;
            } else {
                // Invalid date - red border, show error message
                dobInput.className = 'datepicker-input invalid';
                validationIndicator.textContent = validation.errors.join('; ');
                validationIndicator.className = 'validation-indicator error';
                continueButton.disabled = true;
            }
        } else {
            // Incomplete - red border, no message
            dobInput.className = 'datepicker-input invalid';
            validationIndicator.textContent = '';
            validationIndicator.className = 'validation-indicator';
            continueButton.disabled = true;
        }
    }


    /**
     * Celebrate with button animation
     */
    celebrate() {
        const continueButton = document.getElementById('continue-button');

        // Store original state
        const originalText = continueButton.textContent;
        const originalBg = continueButton.style.backgroundColor;

        // Change to success state
        continueButton.textContent = 'Well done!';
        continueButton.style.backgroundColor = '#27ae60';
        continueButton.style.cursor = 'pointer';
        continueButton.disabled = true;

        // Reset after 1 second
        setTimeout(() => {
            continueButton.textContent = originalText;
            continueButton.style.backgroundColor = originalBg;
            continueButton.style.cursor = '';
            continueButton.disabled = false;
        }, 1000);
    }

    /**
     * Show success screen
     */
    showSuccess(validation, expressions) {
        this.showScreen('success');

        // Fill in success details
        document.getElementById('success-date').textContent = validation.date;
        document.getElementById('success-year-expr').textContent = expressions.year;
        document.getElementById('success-year-value').textContent = validation.year;
        document.getElementById('success-month-expr').textContent = expressions.month;
        document.getElementById('success-month-value').textContent = validation.month;
        document.getElementById('success-day-expr').textContent = expressions.day;
        document.getElementById('success-day-value').textContent = validation.day;

        // Setup continue button
        const continueButton = document.getElementById('success-continue');
        continueButton.onclick = () => {
            this.fakeLoadSite();
        };
    }

    /**
     * Show failure screen
     */
    showFailure(message) {
        this.showScreen('failure');

        // Set failure message
        document.getElementById('failure-message').textContent = message;

        // Setup buttons
        const tryAgainButton = document.getElementById('try-again-button');
        const keepGoingButton = document.getElementById('keep-going-button');

        tryAgainButton.onclick = () => {
            this.returnToPuzzle();
        };

        keepGoingButton.onclick = () => {
            this.fakeLoadSite();
        };
    }

    /**
     * Return to puzzle screen
     */
    returnToPuzzle() {
        this.showScreen('puzzle');
    }

    /**
     * Fake loading a site (just shows alert for now)
     */
    fakeLoadSite() {
        alert('Thanks for playing! In a real implementation, this would continue to the actual site.');
    }

    /**
     * Reload workspace (when difficulty changes)
     */
    reloadWorkspace() {
        // Destroy old workspace
        if (this.blocklyManager) {
            this.blocklyManager.destroy();
        }

        // Create new workspace with new numbers
        this.blocklyManager = new BlocklyManager('blockly-workspace', this.gameLogic);
        this.blocklyManager.init();

        // Update controls panel reference to new blocklyManager
        if (this.controlsPanel) {
            this.controlsPanel.blocklyManager = this.blocklyManager;
        }

        // Re-setup live update listener
        this.blocklyManager.workspace.addChangeListener(() => {
            this.updateDateInput();
        });

        // Clear the input and reset to initial state
        const dobInput = document.getElementById('dob-input');
        dobInput.value = '????-??-??';
        dobInput.className = 'datepicker-input invalid';
        const validationIndicator = document.getElementById('validation-indicator');
        validationIndicator.textContent = '';
        validationIndicator.className = 'validation-indicator';
        document.getElementById('continue-button').disabled = true;
    }

    /**
     * Show a specific screen
     */
    showScreen(screenId) {
        // Hide all screens
        const screens = document.querySelectorAll('.screen');
        screens.forEach(screen => {
            screen.classList.remove('active');
        });

        // Show target screen
        const targetScreen = document.getElementById(`${screenId}-screen`);
        if (targetScreen) {
            targetScreen.classList.add('active');
            this.currentScreen = screenId;
        }
    }
}

// Initialize application when DOM is ready
document.addEventListener('DOMContentLoaded', () => {
    console.log('DOM Content Loaded - Initializing Countdown Datepicker');

    // Check if Blockly is available
    if (typeof Blockly === 'undefined') {
        console.error('Blockly is not loaded!');
        alert('Blockly library failed to load. Please check your internet connection and refresh.');
        return;
    }

    console.log('Blockly loaded successfully');

    const app = new CountdownDatepicker();
    app.init();
});
