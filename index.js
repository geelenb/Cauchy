Polynomial.setField('C');

let scale = 250;
const sharpness = 3;
const animation_speed = 1 / 4; // = revolutions per second in 'orbit' mode
const contour_length = 500;
const in_ctx = document.getElementById('input_canvas').getContext('2d');
const out_ctx = document.getElementById('output_canvas').getContext('2d');
const rl_ctx = document.getElementById('rlocus_canvas').getContext('2d');
const krange = document.getElementById('krange');


const zeros = [Complex(-2)];
const poles = [Complex(-1, 1), Complex(-1, -1)];
let K = 1;

const mul = (a, b) => a.mul(b);

function f(s) {
    const counter = zeros.map(z => s.sub(z)).reduce(mul, Complex.ONE);
    const denominator = poles.map(p => s.sub(p)).reduce(mul, Complex.ONE);
    return counter.mul(K).div(denominator);
}

function add_drag(ctx) {
    ctx.canvas.addEventListener('mousedown', () => ctx.canvas.isdragging = false);

    ctx.canvas.addEventListener('mousemove', (event) => {
        if (event.buttons) {
            const dx = event.movementX * sharpness;
            const dy = event.movementY * sharpness;
            if (ctx === in_ctx || ctx === rl_ctx) {
                in_ctx.translate(dx, dy);
                rl_ctx.translate(dx, dy);
                IN.redraw();
                RL.redraw();
            } else {
                ctx.translate(dx, dy);
            }
            if (ctx === out_ctx) {
                OUT.redraw();
            }

            ctx.canvas.isdragging = true;
        }
    });
}

add_drag(in_ctx);
add_drag(out_ctx);
add_drag(rl_ctx);

function add_axes(ctx) {
    const axes_length = 10;

    ctx.save();
    ctx.font = '14px serif';

    ctx.strokeStyle = 'black';
    ctx.lineWidth = sharpness * 1;

    ctx.beginPath();
    ctx.moveTo(-1000 * sharpness * scale, 0);
    ctx.lineTo(+1000 * sharpness * scale, 0);
    ctx.stroke();

    ctx.beginPath();
    ctx.moveTo(0, -1000 * sharpness);
    ctx.lineTo(0, +1000 * sharpness);
    ctx.stroke();

    ctx.font = `${10 * sharpness}pt serif`;
    const x = -10 * sharpness;
    const y = 10 * sharpness;
    // labels
    for (let i = -axes_length; i <= axes_length; i++) {
        if (i === 0) {
            continue;
        }

        // x axis
        ctx.beginPath();
        ctx.moveTo(i * scale, 0);
        ctx.lineTo(i * scale, 6 * sharpness);
        ctx.stroke();

        ctx.textAlign = 'center';
        ctx.textBaseline = 'hanging';
        ctx.fillText(`${i}`, i * scale, y);

        // y axis
        ctx.beginPath();
        ctx.moveTo(0, i * scale);
        ctx.lineTo(-6 * sharpness, i * scale);
        ctx.stroke();

        ctx.textAlign = 'right';
        ctx.textBaseline = 'middle';
        ctx.fillText(`${i}j`, x, -i * scale);
    }

    ctx.textAlign = 'right';
    ctx.textBaseline = 'hanging';
    ctx.fillText('0', x, y);

    ctx.restore();
}

function headpoint(points) {
    if (mode === 'interactive') {
        return points[points.length - 1];
    } else {
        return points[math.round(new Date().getTime() * animation_speed * contour_length / 1000) % points.length];
    }
}

function draw_points(ctx, points) {
    // TODO: move inside classes?
    if (points.length === 0) {
        return;
    }

    points = points.map(s => [s.re * scale, -s.im * scale]);

    ctx.save();
    ctx.beginPath();

    ctx.strokeStyle = 'cyan';
    ctx.lineWidth = sharpness * 1;
    ctx.moveTo(...(points[0]));
    points.forEach(point => {
        ctx.lineTo(...point);
    });

    ctx.stroke();

    const point = headpoint(points);
    ctx.fillStyle = ctx.strokeStyle;
    ctx.beginPath();
    ctx.arc(point[0], point[1], 3 * sharpness, 0, math.tau);
    ctx.fill();

    ctx.restore();
}

function draw_poles(ctx) {
    const size = 3 * sharpness;
    ctx.save();
    ctx.strokeStyle = 'blue';
    ctx.lineWidth = sharpness * 1;
    poles.forEach(p => {
        ctx.beginPath();
        ctx.moveTo(p.re * scale - size, -(p.im * scale - size));
        ctx.lineTo(p.re * scale + size, -(p.im * scale + size));
        ctx.stroke();

        ctx.beginPath();
        ctx.moveTo(p.re * scale - size, -(p.im * scale + size));
        ctx.lineTo(p.re * scale + size, -(p.im * scale - size));
        ctx.stroke();
    });

    ctx.restore();
}

function draw_zeros(ctx) {
    const r = 5 * sharpness;
    ctx.save();
    ctx.strokeStyle = 'blue';
    ctx.lineWidth = sharpness * 1;
    zeros.forEach(p => {
        ctx.beginPath();
        // ctx.moveTo(p.re * scale, -(p.im * scale));
        ctx.arc(p.re * scale, -(p.im * scale), r, 0, Math.PI * 2);
        ctx.stroke();
    });

    ctx.restore();
}

class InputCanvas {
    constructor(ctx) {
        this.ctx = ctx;
        const canvas = ctx.canvas;

        this.contour = [];

        const pole_or_zero_clicked = (event) => {
            const ctx = this.ctx;
            if (ctx.canvas.isdragging) {
                return;
            }

            let poles_or_zeros = event.type === 'contextmenu' ? poles : zeros;
            let clicked = this.coor_for_event(event);

            // remove poles_or_zeros that are close to click
            const pz_to_keep = poles_or_zeros.filter(z => clicked.sub(z).abs() > .2 && clicked.sub(z.conjugate()).abs() > .2);

            if (pz_to_keep.length !== poles_or_zeros.length) {
                // remove the poles_or_zeros in-place:
                poles_or_zeros.splice(0, poles_or_zeros.length, ...pz_to_keep);
            } else {
                // if no poles_or_zeros were removed: add one
                clicked = Complex(
                    round_n(clicked.re, 1),
                    round_n(clicked.im, 1)
                );
                poles_or_zeros.push(clicked);
                if (clicked.im !== 0) {
                    poles_or_zeros.push(clicked.conjugate())
                }
            }

            RL.recalculate_accurately();
            IN.redraw();
            OUT.recalculate();
            OUT.redraw();
            G_desc.update();
            bode.recalculate();
            bode.redraw();
            PZT.update();

            event.preventDefault();
        };

        canvas.addEventListener('click', pole_or_zero_clicked);
        canvas.addEventListener('contextmenu', pole_or_zero_clicked);

        canvas.addEventListener('mousemove', (event) => {
            if (window.mode === 'interactive') {
                const coor = this.coor_for_event(event, this.ctx);
                this.contour.push(coor);
                OUT.recalculate();

                IN.redraw();
                OUT.redraw();
            }
        });
    }

    redraw() {
        const ctx = this.ctx;
        const contour = this.contour;

        const draw_phasors = () => {
            if (contour.length === 0) {
                return;
            }

            const s = headpoint(contour);

            ctx.save();
            const r = 10 * sharpness;
            ctx.lineWidth = sharpness * 1;
            ctx.strokeStyle = 'lightgrey';
            poles.concat(zeros).forEach(c => {
                ctx.beginPath();
                ctx.moveTo(s.re * scale, -s.im * scale);
                ctx.lineTo(c.re * scale, -c.im * scale);
                ctx.stroke();
            });

            ctx.strokeStyle = 'red';
            ctx.lineWidth = sharpness * 1;
            poles.forEach(p => {
                ctx.beginPath();
                ctx.arc(p.re * scale, -p.im * scale, r, -s.sub(p).arg(), 0);
                ctx.stroke();
            });

            ctx.strokeStyle = 'darkgreen';
            ctx.lineWidth = sharpness * 1;
            zeros.forEach(z => {
                ctx.beginPath();
                ctx.arc(z.re * scale, -z.im * scale, r, -s.sub(z).arg(), 0);
                ctx.stroke();
            });

            ctx.restore()
        };

        ctx.clearRect(-1000 * sharpness, -1000 * sharpness, 2000 * sharpness, 2000 * sharpness);
        add_axes(ctx);

        draw_points(ctx, this.contour);

        draw_phasors();

        draw_poles(ctx);
        draw_zeros(ctx);
    }

    coor_for_event(event) {
        const rect = this.ctx.canvas.getBoundingClientRect();
        const point_unscaled = (this.ctx
                .getTransform()
                .inverse()
                .transformPoint({
                    x: (event.clientX - rect.left) * sharpness,
                    y: (event.clientY - rect.top) * sharpness
                })
        );

        return Complex(
            point_unscaled.x,
            -point_unscaled.y
        ).div(scale)
    }

    circle_around(p) {
        return math.range(0, math.tau, math.tau / contour_length).map(a => p.add({arg: -a, abs: 0.8})).toArray();
    }

    nyquist_contour() {
        const omegas = logspace_1(2, contour_length / 2)
        return omegas.concat(...omegas.map(x => -1 / x)).map(o => Complex(0, o));
    }

    set_mode(new_mode) {
        if (new_mode === 'interactive') {
            this.contour = [];
        } else if (new_mode === 'origin') {
            this.contour = [Complex(0, 0)];
        } else if (new_mode === 'orbit_zero') {
            this.contour = this.circle_around(zeros[math.randomInt(zeros.length)]);
        } else if (new_mode === 'orbit_pole') {
            this.contour = this.circle_around(poles[math.randomInt(poles.length)]);
        } else if (new_mode === 'nyquist') {
            this.contour = this.nyquist_contour();
        }
    }

    center() {
        this.ctx.translate(out_ctx.canvas.height / 1.3, out_ctx.canvas.height / 2);
    }
}

class OutputCanvas {
    constructor(ctx) {
        this.ctx = ctx;
        this.plot = [];
    }

    recalculate() {
        this.plot = IN.contour.map(f);
    }

    draw_phasors() {
        const ctx = this.ctx;
        if (this.plot.length === 0) {
            return;
        }
        let sum_phase = 0;
        let r = 10 * sharpness;

        const s = headpoint(IN.contour);
        ctx.save();

        ctx.strokeStyle = 'lightgrey';
        ctx.lineWidth = 1 * sharpness;
        const fs = headpoint(this.plot);

        ctx.beginPath();
        ctx.moveTo(0, 0);
        ctx.lineTo(fs.re * scale, -fs.im * scale);
        ctx.stroke();

        ctx.strokeStyle = 'darkgreen';

        zeros.forEach(z => {
            let phase = -s.sub(z).arg();

            ctx.beginPath();
            ctx.arc(0, 0, r, phase + sum_phase, sum_phase);
            ctx.stroke();

            sum_phase += phase;
            r += 5 * sharpness;
        });

        ctx.strokeStyle = 'red';
        poles.forEach(p => {
            let phase = s.sub(p).arg();

            ctx.beginPath();
            ctx.arc(0, 0, r, sum_phase, phase + sum_phase);
            ctx.stroke();

            sum_phase += phase;
            r += 5 * sharpness;
        });

        ctx.restore()
    };

    redraw() {
        const ctx = this.ctx;
        ctx.clearRect(-1000 * sharpness, -1000 * sharpness, 2000 * sharpness, 2000 * sharpness);
        add_axes(ctx);

        draw_points(ctx, this.plot);

        this.draw_phasors();
    }

    center() {
        this.ctx.translate(out_ctx.canvas.height / 2, out_ctx.canvas.height / 2);
    }
}

class RootLocus {
    constructor(ctx) {
        this.ctx = ctx;
        this.root_lines = [];
    }

    complex_rand() {
        return Complex(math.random() - .5, math.random() - .5);
    }

    recalculate_quickly() {
        if (zeros.length <= 2 && poles.length <= 2) {
            this.recalculate_root_lines(1000, 1.5);
        } else {
            const max_exponent = math.sqrt(math.max(zeros.length, poles.length));
            this.recalculate_root_lines(1000, max_exponent);
        }
    }

    recalculate_accurately() {
        if (zeros.length <= 2 && poles.length <= 2) {
            this.recalculate_root_lines(10000, 1.5);
        } else {
            const max_exponent = math.sqrt(math.max(zeros.length, poles.length));
            this.recalculate_root_lines(3000, max_exponent);
        }
    }

    recalculate_root_lines(n_ks, max_exponent) {
        // 1 + KP = 0
        // P = counter / denominator
        // denominator + K counter = 0

        const counter = Polynomial.fromRoots(zeros).monic();
        const denominator = Polynomial.fromRoots(poles).monic();
        const order = Math.max(zeros.length, poles.length);

        let roots_per_K;

        const max_diff_abs = (l) => math.max(l[l.length - 1].map(
            (root, i) => root.sub(l[l.length - 2][i]).abs()));


        if (order === 1) {
            roots_per_K = [
                [zeros[0] || Complex(-1000)],
                [poles[0] || Complex(-1000)]
            ];
        } else if (order === 2) {
            const wanted_abs_diff = .05;
            roots_per_K = [];

            const coeff_full = (poly, n) => new Array(n).fill().map(
                (_, i) => (poly.coeff[i] || {}).re || 0);
            const counter_arr = coeff_full(counter, 3);
            const denominator_arr = coeff_full(denominator, 3);

            // use quadratic formula to estimate roots
            const max_k = math.pow(10, max_exponent);

            let k_multiplier = math.pow(10, max_exponent * 2 / n_ks);

            for (let k = 1 / max_k, i = 0; k < max_k; k *= k_multiplier, i++) {
                const a = counter_arr[2] * k + denominator_arr[2];
                const b = counter_arr[1] * k + denominator_arr[1];
                const c = counter_arr[0] * k + denominator_arr[0];

                const D = Complex(b * b - 4 * a * c);
                roots_per_K.push([
                    D.sqrt().add(-b).mul(1 / 2 / a),
                    D.sqrt().add(b).mul(-1 / 2 / a)
                ]);

                if (i > 0) {
                    // console.log('i', i);
                    // console.log('wanted_abs_diff', wanted_abs_diff);
                    // console.log('max_diff_abs', max_diff_abs());

                    // console.log('/', wanted_abs_diff / max_diff_abs());
                    // const abs_diff = max_diff_abs(roots_per_K)
                    // k_multiplier *= math.pow(abs_diff / wanted_abs_diff, .001);
                    // k_multiplier = math.max(1.0001, k_multiplier); // multiplier must always > 1

                    // console.log(math.log10(k_multiplier));

                }
            }

            console.log('roots_per_K.length', roots_per_K.length)

        } else {
            console.log('calculating root lines using newton-rhapson');
            roots_per_K = new Array(n_ks);

            const calculate_roots = (poly, est) => {
                let roots = [];
                poly = poly.monic();
                while (poly.coeff[0].abs() === 0) {
                    roots.push(Complex.ZERO);
                    poly = poly.div('x').monic();
                }

                if (est === undefined) {
                    est = Array(poly.degree()).fill().map(this.complex_rand)
                }

                poly.complexRoots(est).root.forEach(root => roots.push(root));

                return roots
            };

            const ks = logspace_1(max_exponent, n_ks);

            // we estimate the roots using Newton Rhapson
            // use roots at previous K value as initial guess
            //
            roots_per_K[0] = calculate_roots(counter.mul(ks[0]).add(denominator));
            for (let i = 1; i < n_ks; i++) {
                let estimated_roots = roots_per_K[i - 1];
                roots_per_K[i] = calculate_roots(
                    counter.mul(ks[i]).add(denominator),
                    estimated_roots
                );
            }
        }

        this.root_lines = roots_per_K[0].map(() => []);
        roots_per_K.forEach(roots =>
            roots.forEach((root, i) =>
                this.root_lines[i].push(root)));

        this.redraw();
    }

    redraw() {
        const ctx = this.ctx;
        const draw_closed_loop_poles = () => {
            // 1 + KP = 0
            // P = counter / denominator
            // denominator + K counter = 0

            const counter = Polynomial.fromRoots(zeros);
            const denominator = Polynomial.fromRoots(poles);

            const poly = counter.mul(K).add(denominator).monic();

            const est = Array(poly.degree()).fill(0).map(this.complex_rand);
            poly.complexRoots(est).root.forEach(root => {
                let [x, y] = root.mul(scale).toVector();

                ctx.save();
                // ctx.strokeStyle = 'black';
                ctx.lineWidth = sharpness * 2;

                let d = .1 * scale;

                ctx.beginPath();
                ctx.moveTo(x - d, y);
                ctx.lineTo(x + d, y);
                ctx.stroke();

                ctx.beginPath();
                ctx.moveTo(x, y - d);
                ctx.lineTo(x, y + d);
                ctx.stroke();

                ctx.restore();
            });

        };
        const draw_root_lines = () => {
            ctx.save();
            ctx.strokeStyle = 'blue';
            ctx.fillStyle = 'blue';
            ctx.lineWidth = sharpness * 2;

            this.root_lines.forEach(root_line => {
                const points = root_line.map(s => s.mul(scale).toVector());

                if (true) {
                    // draw lines
                    ctx.beginPath();
                    ctx.moveTo(...(points[0]));
                    points.forEach(point => {
                        ctx.lineTo(...point);
                    });
                    ctx.stroke();
                } else {
                    // draw dots

                    points.forEach(point => {
                        ctx.fillStyle = ctx.strokeStyle;
                        ctx.beginPath();
                        ctx.arc(point[0], point[1], 2 * sharpness, 0, math.tau);
                        ctx.fill();
                    });
                }


            });
            ctx.restore();
        };


        ctx.clearRect(-1000 * sharpness, -1000 * sharpness, 2000 * sharpness, 2000 * sharpness);
        add_axes(ctx);

        draw_root_lines();
        draw_closed_loop_poles();

        draw_poles(ctx);
        draw_zeros(ctx);
    }

    center() {
        this.ctx.translate(out_ctx.canvas.height / 1.3, out_ctx.canvas.height / 2);
    }
}

class BodePlot {
    constructor(ctx) {
        this.tick_length = 10;

        this.ctx = ctx;

        this.min_omega_pow = -2;
        this.max_omega_pow = 2;
        const n_x = 100;

        this.omega_pows = math.range(
            this.min_omega_pow,
            this.max_omega_pow,
            (this.max_omega_pow - this.min_omega_pow) / n_x
        ).toArray().concat([this.max_omega_pow]);

        this.tick_pows = math.range(this.min_omega_pow, this.max_omega_pow + 1, 1).toArray();
        this.omega_js = this.omega_pows.map(x => Complex(0, math.pow(10, x)));

        const superchar = i => i == 1 ? '' : i < 4 ? String.fromCodePoint(176 + i) : String.fromCodePoint(8304 + math.abs(i));
        const pow_to_str = o => o == 0 ? '1' : (o < 0 ? '0.1' : '10') + superchar(math.abs(o));
        this.tick_labels = this.tick_pows.map(pow_to_str);

        this.recalculate();
    }

    complex_db(c) {
        return 20 * math.log(c.abs())
    }

    complex_deg(c) {
        return 180 * c.arg() / math.PI;
    }

    unwrap_phases_inplace(degs) {
        let running_add = 0;
        for (let i = 1; i < degs.length; i++) {
            degs[i] += running_add;
            while (degs[i] - degs[i - 1] > 180) {
                degs[i] -= 360;
                running_add -= 360;
            }
            while (degs[i] - degs[i - 1] < -180) {
                degs[i] += 360;
                running_add += 360;
            }
        }
    }

    unwrap_phases(degs) {
        const r = Array.from(degs);
        this.unwrap_phases_inplace(r);
        return r;
    }

    recalculate() {
        const ctx = this.ctx;

        const plot = this.omega_js.map(f);
        const dbs = plot.map(this.complex_db);

        let phases = plot.map(this.complex_deg);
        this.unwrap_phases_inplace(phases);

        const min_phase = math.min(...phases, 180);
        const max_phase = math.max(...phases, 180);
        const min_db = math.min(...dbs, 1);
        const max_db = math.max(...dbs, 1);

        this.h = ctx.canvas.height;
        this.w = ctx.canvas.width;

        this.xmin = .12 * this.w;
        this.xmax = .95 * this.w;

        this.mag_top = ctx.lineWidth;
        this.mag_bottom = .47 * this.h;

        this.phase_top = .53 * this.h;
        this.phase_bottom = this.h - ctx.lineWidth;

        const omega_pow_to_x_scale = (this.xmax - this.xmin) / (this.max_omega_pow - this.min_omega_pow);
        this.omega_pow_to_x = (o) => this.xmin + (o - this.min_omega_pow) * omega_pow_to_x_scale;

        this.x_ticks = this.tick_pows.map(this.omega_pow_to_x);
        this.x_data = this.omega_pows.map(this.omega_pow_to_x);

        this.db_to_y = (sb) => this.mag_bottom - .05 * (this.mag_bottom - this.mag_top) -
            (sb - min_db) * .9 * (this.mag_bottom - this.mag_top) / (max_db - min_db);

        this.deg_to_y = (deg) => this.phase_bottom - .05 * (this.phase_bottom - this.phase_top) -
            (deg - min_phase) * .9 * (this.phase_bottom - this.phase_top) / (max_phase - min_phase);

        this.db_data_y = dbs.map(this.db_to_y);
        const db_ticks = [
            ...math.range(0, max_db, 20).toArray(),
            ...math.range(0, min_db, -20).toArray().slice(1)
        ];
        this.db_ticks_y = db_ticks.map(this.db_to_y);
        this.db_ticks_labels = db_ticks.map(db => `${db}dB`);

        // yticks deg
        const deg_ticks = [
            ...math.range(0, max_phase * 1.05, 45).toArray(),
            ...math.range(0, min_phase * 1.01, -45).toArray().slice(1)
        ];
        this.deg_ticks_ys = deg_ticks.map(this.deg_to_y);
        this.deg_ticks_labels = deg_ticks.map(deg => `${deg}°`);

        this.y_phase_data = phases.map(this.deg_to_y);
    }

    draw_rectangles() {
        const ctx = this.ctx;
        ctx.rect(this.xmin, this.mag_top, this.xmax - this.xmin, this.mag_bottom - this.mag_top);
        ctx.stroke();

        ctx.rect(this.xmin, this.phase_top, this.xmax - this.xmin, this.phase_bottom - this.phase_top);
        ctx.stroke();
    };

    draw_x_ticks() {
        const ctx = this.ctx;
        ctx.font = `${10 * sharpness}pt serif`;
        ctx.textAlign = 'center';
        ctx.textBaseline = 'middle';

        this.x_ticks.forEach((x, i) => {
            ctx.moveTo(x, this.mag_bottom);
            ctx.lineTo(x, this.mag_bottom + this.tick_length);

            ctx.fillText(this.tick_labels[i], x, this.h / 2);

            ctx.moveTo(x, this.phase_top);
            ctx.lineTo(x, this.phase_top - this.tick_length);
        });
        ctx.stroke();
    }

    draw_data(xs, ys) {
        const ctx = this.ctx;
        ctx.moveTo(xs[0], ys[0]);
        ys.forEach((y, i) => ctx.lineTo(xs[i], y));
        ctx.stroke();
    }

    draw_yticks(ys, labels) {
        const ctx = this.ctx;
        ctx.textAlign = 'right';
        ctx.textBaseline = 'middle';

        ys.forEach((y, i) => {
            ctx.moveTo(this.xmin, y);
            ctx.lineTo(this.xmin - this.tick_length, y);

            ctx.fillText(labels[i], this.xmin - this.tick_length - 5, y);
        });
        ctx.stroke();
    }

    add_nyquist_helplines() {
        const ctx = this.ctx;
        ctx.save();

        const s = headpoint(IN.contour);
        const p = f(Complex(0, s.abs()));
        const x = this.omega_pow_to_x(math.log10(s.abs() + .0001));
        const deg_y = this.deg_to_y(this.complex_deg(p));
        const amp_y = this.db_to_y(this.complex_db(p));

        ctx.strokeStyle = 'lightgray';

        ctx.beginPath();
        ctx.moveTo(x, this.mag_top);
        ctx.lineTo(x, this.mag_bottom);

        ctx.moveTo(x, this.phase_top);
        ctx.lineTo(x, this.phase_bottom);
        ctx.stroke();

        ctx.beginPath();
        ctx.moveTo(this.xmin, deg_y);
        ctx.lineTo(x, deg_y);
        ctx.stroke();

        ctx.beginPath();
        ctx.moveTo(this.xmin, amp_y);
        ctx.lineTo(x, amp_y);
        ctx.stroke();

        ctx.restore();
    }

    add_sketch_lines() {
        // todo: "running" sketch
        // Type of the system l
        // const l = poles.filter(p => p.equals(Complex.ZERO)).length -
        //     zeros.filter(z => z.equals(Complex.ZERO)).length;

        const pz_merged_sorted = [...poles, ...zeros];

        const ctx = this.ctx;
        ctx.save();
        ctx.strokeStyle = 'gray';
        ctx.lineWidth = .5 * sharpness;
        pz_merged_sorted.forEach(p => {
            const omega_n = p.abs();
            if (p.im < 0) {
                return;
            }

            // todo: kleur (pool: rood, np: blauw)

            const x = this.omega_pow_to_x(math.log10(omega_n));
            ctx.beginPath();
            ctx.moveTo(x, this.phase_bottom);
            ctx.lineTo(x, this.phase_top);
            ctx.stroke();

            ctx.beginPath();
            ctx.moveTo(x, this.mag_bottom);
            ctx.lineTo(x, this.mag_top);
            ctx.stroke();
        });
        ctx.restore();
    }

    redraw() {
        const ctx = this.ctx;

        ctx.clearRect(-1000 * sharpness, -1000 * sharpness, 2000 * sharpness, 2000 * sharpness);

        ctx.lineWidth = sharpness * 1;

        ctx.save();

        ctx.beginPath();
        ctx.strokeStyle = 'black';

        this.draw_rectangles();
        this.draw_x_ticks();

        this.draw_yticks(this.db_ticks_y, this.db_ticks_labels);
        this.draw_data(this.x_data, this.db_data_y);

        this.draw_yticks(this.deg_ticks_ys, this.deg_ticks_labels);
        this.draw_data(this.x_data, this.y_phase_data);

        if (mode === 'nyquist') {
            this.add_nyquist_helplines();
        } else {
            this.add_sketch_lines();
        }

        ctx.restore()
    }
}

class GDescription {
    constructor(elem_to_fill) {
        this.root = elem_to_fill;
    }

    update_K() {
        let K_str = K === 1 ? '' : String(round_n(K, 2));
        Array.from(document.getElementsByClassName('K_val')).forEach(elem => elem.innerText = K_str);
    }

    update() {
        const roots_to_html = roots => {
            if (roots.length == 0) {
                return '1';
            }
            const complex_to_str = c => (c.re < 0 || (c.re == 0 && c.im < 0)) ? c.toString() : ('+' + c.toString());
            const complex_to_s_minus = c => Complex.ZERO.equals(c) ? 's' : '(s' + complex_to_str(Complex(0).sub(c)) + ')';
            const add_spacing = s => s.replaceAll('i', 'j').replaceAll('+', '&nbsp+&nbsp').replaceAll('-', '&nbsp-&nbsp');

            // TODO : powers for double roots?

            let html = roots.map(complex_to_s_minus).map(add_spacing).join('&nbsp;');

            if (roots.length == 1) {
                html = html.substr(1, html.length - 2)
            }

            return html
        };

        const roots_to_html_poly = roots => {
            if (roots.length === 0) {
                return '1';
            }
            const poly = Polynomial.fromRoots(roots);
            const coeffs = poly.monic().coeff;

            const s_pow = (pow) => pow === '0' ? '' : pow === '1' ? 's' : `s<sup>${pow}</sup>`;

            return Object.entries(coeffs).reverse().map(([pow, coeff], i) =>
                `${coeff === 1 ? '' : round_n(coeff, 3)}${s_pow(pow)}`
            ).join('&nbsp;+&nbsp;').replaceAll('+&nbsp;-', '-&nbsp;').replaceAll('i', 'j');
        };

        let counter_roots = roots_to_html(zeros);
        let counter_polynomic = roots_to_html_poly(zeros);
        let denominator_roots = roots_to_html(poles);
        let denominator_polynomic = roots_to_html_poly(poles);

        this.root.innerHTML = [
            '<table class="equationtable"><tr>',
            '<td rowspan="2">KG(s)</td>',
            '<td rowspan="2" >=&nbsp; <span class="K_val"></span> </td>',
            `<td class="counter">&nbsp;${counter_roots}&nbsp;</td>`,
            '</tr>',
            '<tr>',
            `<td>&nbsp;${denominator_roots}&nbsp;</td>`,
            '</tr>',
            '<tr style="height: 2ch"></tr>',
            '<tr>',
            '<td rowspan="2"></td>',
            '<td rowspan="2">&nbsp;=&nbsp;<span class="K_val"></span> </td>',
            `<td class="counter">&nbsp;${counter_polynomic}&nbsp;</td>`,
            '</tr>',
            '<tr>',
            `<td>&nbsp;${denominator_polynomic}&nbsp;</td>`,
            '</tr>',
            '</table>'
        ].join('');
        this.update_K();
    }
}

class PZ_Table {
    constructor(elem_to_fill) {
        this.root = elem_to_fill;
    }

    update() {
        const table_for_list = (list) => {
            const details_list = list.filter(p => p.im >= 0).map(p => {
                const details = document.createElement('details');
                // details.open = true;
                const arg = p.arg() / math.PI * 180;
                if (p.im === 0) {
                    details.innerHTML = `
                        <summary>${p.toString()}</summary>
                        <div>
                        <p>Value</p>
                        <input type="range" value="${p.re}" concept="re" min="-5" max="5" step=".1">
                        <input type="text" value="${p.re}" concept="re">
                        </div>`;
                } else {
                    const summary = p.toString().replace('+', '±');
                    details.innerHTML = `
                        <summary>${summary}</summary>
                        <div>
                        <p>Real</p>
                        <input type="range" value="${p.re}" concept="re" min="-5" max="5" step=".1">
                        <input type="text" value="${p.re}" concept="re">
                        <p>Imag</p>
                        <input type="range" value="${p.im}" concept="im" min="0" max="10" step=".1">
                        <input type="text" value="${p.im}" concept="im">
                        <p>Mod</p>
                        <input type="range" value="${p.abs()}" concept="abs" min="0" max="10" step=".1">
                        <input type="text" value="${p.abs()}" concept="abs">
                        <p>Arg</p>
                        <input type="range" value="${arg}" concept="arg" min="0" max="180" step="1">
                        <input type="text" value="${arg}" concept="arg"></div>`;
                }
                details.querySelectorAll('input').forEach(input => {
                    input.p = p;
                });

                details.p = p;

                return details;
            });

            list.filter(p => p.im < 0).forEach(p => {
                const details = details_list.find(summ => summ.p.conjugate().equals(p));
                details.querySelectorAll('input').forEach(input => {
                    input.conj = p;
                });
            });

            return details_list;
        };

        this.root.innerHTML = '<h3>Zeros</h3>';
        table_for_list(zeros).forEach(t => this.root.appendChild(t));
        this.root.insertAdjacentHTML('beforeend', '<h3>Poles</h3>');
        table_for_list(poles).forEach(t => this.root.appendChild(t));

        this.add_eventlisteners();
    }

    add_eventlisteners() {
        const set_complex = (old, new_) => {
            old.re = new_.re;
            old.im = new_.im;
        };

        this.root.querySelectorAll('input').forEach(input => {
            const details = input.closest('details');
            const summary = details.getElementsByTagName('summary')[0];
            input.addEventListener('input', event => {
                if (input.type === 'range') {
                    input.nextElementSibling.value = input.value;
                } else if (input.type === 'text') {
                    input.previousElementSibling.value = input.value;
                }

                const concept = input.attributes['concept'].value;
                if (input.conj !== undefined) {
                    let new_p;
                    // pole or zero PAIR; update both
                    if (concept == 'abs' || concept == 'arg') {
                        new_p = Complex({
                            arg: details.querySelector('input[concept=arg]').value * math.PI / 180,
                            abs: details.querySelector('input[concept=abs]').value,
                        });
                        details.querySelectorAll('input[concept=re]').forEach(input => input.value = new_p.re);
                        details.querySelectorAll('input[concept=im]').forEach(input => input.value = new_p.im);
                    } else {
                        new_p = Complex(
                            details.querySelector('input[concept=re]').value,
                            details.querySelector('input[concept=im]').value,
                        );
                        details.querySelectorAll('input[concept=arg]').forEach(input => input.value = new_p.arg() * 180 / math.PI);
                        details.querySelectorAll('input[concept=abs]').forEach(input => input.value = new_p.abs());
                    }

                    set_complex(input.p, new_p);
                    set_complex(input.conj, new_p.conjugate());
                    summary.innerHTML = new_p.toString().replace('+', '±');
                } else {
                    input.p.re = parseFloat(input.value);
                    summary.innerHTML = input.value;
                }

                if (input.type === 'range') {
                    RL.recalculate_quickly();
                } else {
                    // text input
                    RL.recalculate_accurately();
                }
                IN.redraw();
                OUT.recalculate();
                OUT.redraw();
                G_desc.update();
                bode.recalculate();
                bode.redraw();
            });

            if (input.type === 'range') {
                input.addEventListener('mouseup', () => {
                    // slider stopped moving
                    RL.recalculate_accurately();
                })
            }
        })
    }
}

/**
 * Round to n decimals after point
 * @param x
 * @param n decimals
 * @returns {number}
 */
function round_n(x, n) {
    let p = Math.pow(10, n);
    return Math.round(x * p) / p
}

function logspace_1(max_10_pow, n) {
    return new Array(n).fill().map((_, i) => Math.pow(10, max_10_pow * (2 * i / (n - 1) - 1)))
}

function start_animation() {
    if (window.mode === 'interactive') {
        return;
    }

    IN.redraw();
    OUT.redraw();
    bode.redraw();

    requestAnimationFrame(start_animation);
}

function set_mode(new_mode) {
    const was_not_animating = window.mode === 'interactive' || window.mode === 'origin';
    window.mode = new_mode;

    IN.set_mode(new_mode);

    OUT.recalculate();

    if (was_not_animating) {
        start_animation();
    }
}

const IN = new InputCanvas(in_ctx);
const OUT = new OutputCanvas(out_ctx);
const RL = new RootLocus(rl_ctx);
const G_desc = new GDescription(document.getElementById('G_equation'));
const bode = new BodePlot(document.getElementById('bode_canvas').getContext('2d'));
const PZT = new PZ_Table(document.getElementById('pz_table'));

krange.addEventListener('input', () => {
    // K changed
    K = Math.pow(10, krange.value);
    document.getElementById('k_pow_span').innerText = krange.value;

    OUT.recalculate();
    G_desc.update_K();
    bode.recalculate();

    OUT.redraw();
    RL.redraw();
    bode.redraw();
});

document.querySelectorAll('input[name=interactiontype]').forEach(i => {
    i.addEventListener('click', () => {
            set_mode(i.dataset.mode);
        }
    );
});

function layoutchanged() {
    let canvases = document.querySelectorAll('canvas');
    canvases.forEach(canvas => {
        let w = canvas.parentElement.clientWidth;
        canvas.width = w * sharpness;
        canvas.height = w * sharpness;
        canvas.style.width = w + 'px';
        canvas.style.height = w + 'px';
    });

    IN.center();
    OUT.center();
    RL.center();

    bode.recalculate();

    IN.redraw();
    OUT.redraw();
    RL.redraw();
    bode.redraw();
}

window.addEventListener('resize', layoutchanged);

set_mode('origin');
// set_mode('nyquist'); // will not start animation on first call!
// start_animation();

RL.recalculate_accurately();
G_desc.update();
PZT.update();
layoutchanged(); // calls redraw_all

