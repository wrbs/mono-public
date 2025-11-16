/**
 * Game Logic for Countdown Datepicker
 * Handles CSV loading, expression evaluation, and validation
 */

class GameLogic {
    constructor() {
        this.currentNumbers = [];
        this.solutions = new Map(); // Map of date string -> solution object
        this.currentDifficulty = 'solutions_base_11.csv';

        // Metadata about solution set completeness (99%+ coverage of 1950-2030)
        this.incompleteSetPaths = new Set([
            'solutions/palindromic.csv'  // 99.8% coverage, missing 56 dates
        ]);
    }

    /**
     * Load a CSV file and parse solutions
     */
    async loadSolutions(filename) {
        try {
            const response = await fetch(filename);
            const text = await response.text();

            const lines = text.trim().split('\n');

            // First line contains the number set
            const numberLine = lines[0];
            this.currentNumbers = numberLine.trim().split(/\s+/).map(n => parseInt(n));

            // Parse solution lines (skip header on line 1, start from line 2)
            this.solutions.clear();
            for (let i = 2; i < lines.length; i++) {
                const line = lines[i].trim();
                if (!line) continue;

                const parts = line.split(',');
                if (parts.length < 4) continue;

                const date = parts[0];
                const yearExpr = parts[1];
                const monthExpr = parts[2];
                const dayExpr = parts[3];

                this.solutions.set(date, {
                    date,
                    yearExpr,
                    monthExpr,
                    dayExpr
                });
            }

            this.currentDifficulty = filename;
            return true;
        } catch (error) {
            console.error('Failed to load solutions:', error);
            return false;
        }
    }

    /**
     * Get current number set
     */
    getNumbers() {
        return [...this.currentNumbers];
    }

    /**
     * Get solution for a specific date
     */
    getSolution(dateString) {
        return this.solutions.get(dateString);
    }

    /**
     * Check if current solution set has complete coverage
     */
    isCompleteSet() {
        return !this.incompleteSetPaths.has(this.currentDifficulty);
    }

    /**
     * Get all solutions
     */
    getAllSolutions() {
        return Array.from(this.solutions.values());
    }

    /**
     * Get a random solution with weighted probability based on complexity
     */
    getRandomComplexSolution() {
        const allSolutions = this.getAllSolutions();

        // Calculate complexity (operations) for each solution
        const solutionsWithComplexity = allSolutions.map(sol => {
            const ops = this.calculateComplexity(sol.yearExpr) +
                       this.calculateComplexity(sol.monthExpr) +
                       this.calculateComplexity(sol.dayExpr);
            // Weight is complexity + 1 (so even 0-operation solutions have a chance)
            return { solution: sol, weight: ops + 1 };
        });

        // Calculate total weight
        const totalWeight = solutionsWithComplexity.reduce((sum, item) => sum + item.weight, 0);

        // Pick weighted random
        let random = Math.random() * totalWeight;
        for (const item of solutionsWithComplexity) {
            random -= item.weight;
            if (random <= 0) {
                return item.solution;
            }
        }

        // Fallback (shouldn't happen)
        return allSolutions[Math.floor(Math.random() * allSolutions.length)];
    }

    /**
     * Calculate expression complexity (number of operations)
     */
    calculateComplexity(expr) {
        return (expr.match(/[+\-*\/]/g) || []).length;
    }

    /**
     * Evaluate an expression safely
     * Returns { success: boolean, value: number | null, error: string | null }
     */
    evaluateExpression(expr) {
        try {
            // Remove whitespace
            expr = expr.trim();

            if (!expr) {
                return { success: false, value: null, error: 'Empty expression' };
            }

            // Check for valid characters only
            if (!/^[\d+\-*\/().\s]+$/.test(expr)) {
                return { success: false, value: null, error: 'Invalid characters in expression' };
            }

            // Evaluate using Function constructor (safer than eval in this controlled context)
            const result = Function('"use strict"; return (' + expr + ')')();

            // Check if result is a valid number
            if (typeof result !== 'number' || !isFinite(result)) {
                return { success: false, value: null, error: 'Result is not a valid number' };
            }

            // Check if result is an integer
            if (!Number.isInteger(result)) {
                return { success: false, value: null, error: 'Result must be a whole number' };
            }

            return { success: true, value: result, error: null };
        } catch (error) {
            return { success: false, value: null, error: error.message };
        }
    }

    /**
     * Extract numbers used in an expression
     */
    extractNumbers(expr) {
        // Remove operators and parentheses, split by any non-digit character
        const numbers = expr.match(/\d+/g) || [];
        return numbers.map(n => parseInt(n));
    }

    /**
     * Validate a date attempt
     * Returns { valid: boolean, errors: string[], date: string | null }
     */
    validateDate(yearExpr, monthExpr, dayExpr) {
        const errors = [];

        // Evaluate expressions
        const yearResult = this.evaluateExpression(yearExpr);
        const monthResult = this.evaluateExpression(monthExpr);
        const dayResult = this.evaluateExpression(dayExpr);

        if (!yearResult.success) {
            errors.push(`Year: ${yearResult.error}`);
        }
        if (!monthResult.success) {
            errors.push(`Month: ${monthResult.error}`);
        }
        if (!dayResult.success) {
            errors.push(`Day: ${dayResult.error}`);
        }

        // If any expression failed, return early
        if (errors.length > 0) {
            return { valid: false, errors, date: null };
        }

        const year = yearResult.value;
        const month = monthResult.value;
        const day = dayResult.value;

        // Get current year for contextual messages
        const currentYear = new Date().getFullYear();
        const age = currentYear - year;

        // Check ranges with contextual messages
        if (year > currentYear) {
            errors.push(`Birth year ${year} is in the future`);
        } else if (year < 1900) {
            errors.push(`Birth year ${year} seems unlikely`);
        } else if (age < 18) {
            errors.push(`You must be at least 18 years old`);
        }

        if (month < 1 || month > 12) {
            errors.push(`Month ${month} doesn't exist`);
        }
        if (day < 1 || day > 31) {
            errors.push(`Day ${day} doesn't exist`);
        }

        // Check for number reuse
        const yearNumbers = this.extractNumbers(yearExpr);
        const monthNumbers = this.extractNumbers(monthExpr);
        const dayNumbers = this.extractNumbers(dayExpr);

        const allUsedNumbers = [...yearNumbers, ...monthNumbers, ...dayNumbers];
        const numberCounts = {};

        allUsedNumbers.forEach(num => {
            numberCounts[num] = (numberCounts[num] || 0) + 1;
        });

        // Check each number is available and not overused
        for (const [num, count] of Object.entries(numberCounts)) {
            const numInt = parseInt(num);
            const available = this.currentNumbers.filter(n => n === numInt).length;

            if (available === 0) {
                errors.push(`Number ${num} is not available`);
            } else if (count > available) {
                errors.push(`You've used ${num} too many times`);
            }
        }

        if (errors.length > 0) {
            return { valid: false, errors, date: null };
        }

        // Format date string
        const dateString = `${year}-${String(month).padStart(2, '0')}-${String(day).padStart(2, '0')}`;

        return {
            valid: true,
            errors: [],
            date: dateString,
            year,
            month,
            day
        };
    }

    /**
     * Check if a specific date is in the valid range
     */
    isDateInRange(year, month, day) {
        if (year < 1950 || year > 2030) return false;
        if (month < 1 || month > 12) return false;
        if (day < 1 || day > 31) return false;
        return true;
    }
}

// Export for use in other scripts
window.GameLogic = GameLogic;
