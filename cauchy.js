import Complex from "complex.min.js";

const one = Complex.ONE;
const multiply = (a, b) => a.mul(b);

function make_demo(container) {
    let input_canvas = document.createElement('canvas');
    let output_canvas = document.createElement('canvas');

    let k = 1;
    let zeros = [Complex(0, 0)];
    let poles = [Complex(-3, 3), Complex(-3, -3)];

    const f = (s) => {
        let counter = zeros.map(z => s - z).reduce(multiply, Complex.ONE);
        let denominator  = poles.map(p => s - z).reduce(multiply, Complex.ONE);
        return Complex(k).mul(counter).div(denominator)
    };

    container.appendChild(input_canvas);
    container.appendChild(output_canvas);


    
}