/**
 * Controls Panel
 * Allows viewing solutions, changing difficulty, etc.
 */

class ControlsPanel {
    constructor(gameLogic, blocklyManager, onDifficultyChange, onReset) {
        this.gameLogic = gameLogic;
        this.blocklyManager = blocklyManager;
        this.onDifficultyChange = onDifficultyChange;
        this.onReset = onReset;
    }

    /**
     * Initialize the controls panel
     */
    init() {
        const difficultySelect = document.getElementById('difficulty-select');
        const showSolutionButton = document.getElementById('show-solution-button');
        const showRandomButton = document.getElementById('show-random-solution-button');
        const resetButton = document.getElementById('reset-solution-button');
        const datePicker = document.getElementById('date-picker');

        // Difficulty change
        difficultySelect.addEventListener('change', async (e) => {
            const filename = e.target.value;
            await this.changeDifficulty(filename);
        });

        // Show solution for specific date
        showSolutionButton.addEventListener('click', () => {
            const date = datePicker.value;
            if (date) {
                this.showSolution(date);
            }
        });

        // Show random complex solution
        showRandomButton.addEventListener('click', () => {
            this.showRandomComplexSolution();
        });

        // Reset solution
        resetButton.addEventListener('click', () => {
            this.resetSolution();
        });

        // Set default date to a reasonable value
        const defaultDate = new Date(1995, 0, 15); // 1995-01-15
        datePicker.value = this.formatDateForInput(defaultDate);
    }

    /**
     * Change difficulty (load new CSV)
     */
    async changeDifficulty(filename) {
        const success = await this.gameLogic.loadSolutions(filename);
        if (success) {
            // Update help text based on completeness
            this.updateHelpText();

            // Notify parent to reload workspace
            if (this.onDifficultyChange) {
                this.onDifficultyChange();
            }
        }
    }

    /**
     * Update help text based on whether current set is complete
     */
    updateHelpText() {
        const helpText = document.querySelector('.note');
        if (helpText) {
            const prefix = this.gameLogic.isCompleteSet() ? '(at least)' : '(nearly)';
            helpText.textContent = `${prefix} every date 1950-2030 is solvable`;
        }
    }

    /**
     * Update the numbers display in the instructions panel
     */
    updateNumbersDisplay() {
        const numbersDisplay = document.getElementById('numbers-display');
        if (numbersDisplay) {
            numbersDisplay.textContent = this.gameLogic.getNumbers().join(', ');
        }
    }

    /**
     * Show solution for a specific date
     */
    showSolution(dateString) {
        const solution = this.gameLogic.getSolution(dateString);

        if (!solution) {
            console.log(`No solution found for ${dateString}`);
            // Show brief, visible message
            const datePicker = document.getElementById('date-picker');
            if (datePicker) {
                // Flash the date picker background
                const originalBackground = datePicker.style.backgroundColor;
                datePicker.style.backgroundColor = '#ffebee';

                // Create and show message element
                const messageEl = document.createElement('span');
                messageEl.textContent = 'This date may not be solvable with the current number set';
                messageEl.style.cssText = 'color: #c62828; font-size: 12px; margin-left: 10px; font-style: italic;';
                messageEl.id = 'missing-solution-message';

                // Insert after date picker
                datePicker.parentNode.insertBefore(messageEl, datePicker.nextSibling);

                // Remove after delay
                setTimeout(() => {
                    datePicker.style.backgroundColor = originalBackground;
                    if (messageEl.parentNode) {
                        messageEl.parentNode.removeChild(messageEl);
                    }
                }, 2500);
            }
            return;
        }

        // Build the blocks to show the solution
        this.buildSolutionBlocks(solution);
    }

    /**
     * Show a random complex solution
     */
    showRandomComplexSolution() {
        const solution = this.gameLogic.getRandomComplexSolution();
        if (!solution) {
            return;
        }

        // Update the date picker to this date
        document.getElementById('date-picker').value = solution.date;

        // Build the blocks to show the solution
        this.buildSolutionBlocks(solution);
    }

    /**
     * Reset to empty workspace
     */
    resetSolution() {
        if (!this.blocklyManager) {
            return;
        }

        // Clear everything and recreate just the number blocks
        this.blocklyManager.clearAndRecreateBlocks();

        // Notify that reset is complete
        if (this.onReset) {
            this.onReset();
        }
    }

    /**
     * Format date for input element
     */
    formatDateForInput(date) {
        const year = date.getFullYear();
        const month = String(date.getMonth() + 1).padStart(2, '0');
        const day = String(date.getDate()).padStart(2, '0');
        return `${year}-${month}-${day}`;
    }

    /**
     * Escape HTML to prevent injection
     */
    escapeHtml(text) {
        const div = document.createElement('div');
        div.textContent = text;
        return div.innerHTML;
    }

    /**
     * Build blocks from a solution
     */
    buildSolutionBlocks(solution) {
        if (!this.blocklyManager) {
            return;
        }

        // Clear everything and recreate fresh number blocks
        this.blocklyManager.clearAndRecreateBlocks();

        // Build and connect the expression blocks
        this.blocklyManager.buildExpressions({
            yearExpr: solution.yearExpr,
            monthExpr: solution.monthExpr,
            dayExpr: solution.dayExpr
        });
    }
}

// Export for use in main script
window.ControlsPanel = ControlsPanel;
