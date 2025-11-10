// A simple scrollytelling implementation

// Define the list of chart JSON files that correspond to the steps
const chartSpecs = [
    'initial_cpi_chart_fixed.json', // Corresponds to data-step="0"
    'caption_1_chart_fixed.json'    // Corresponds to data-step="1"
];

const visElement = document.getElementById('vis');
let currentStep = -1; // Keep track of the current step

async function updateChart(stepIndex) {
    // Only update if the step has changed
    if (stepIndex === currentStep) {
        return;
    }
    
    currentStep = stepIndex; // Update the current step
    
    try {
        const specFile = chartSpecs[stepIndex];
        if (!specFile) {
            console.error('No chart spec found for step:', stepIndex);
            return;
        }

        // Fetch the new spec
        const response = await fetch(specFile);
        if (!response.ok) {
            throw new Error(`HTTP error! status: ${response.status} for file ${specFile}`);
        }
        const spec = await response.json();

        // Embed the new spec into the #vis container
        const result = await vegaEmbed(visElement, spec, { actions: false, padding: 15 });
        
    } catch (error) {
        console.error('Error updating chart:', error);
    }
}

// Set up the Intersection Observer
function setupObserver() {
    const options = {
        root: null, // use the viewport
        rootMargin: '0px',
        threshold: 0.5 // Trigger when 50% of the element is in view
    };

    const observer = new IntersectionObserver((entries, observer) => {
        entries.forEach(entry => {
            if (entry.isIntersecting) {
                // Get the step index from the data-step attribute
                const stepIndex = parseInt(entry.target.dataset.step, 10);
                
                // Update the chart
                updateChart(stepIndex);
            }
        });
    }, options);

    // Observe all the .step elements
    const steps = document.querySelectorAll('.step');
    steps.forEach(step => {
        observer.observe(step);
    });
}

// Initialize the chart with the first state (step 0)
updateChart(0);

// Start the observer
setupObserver();