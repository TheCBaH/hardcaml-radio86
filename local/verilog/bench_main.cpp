// Clocks i8080_bench (built with +define+EXTERNAL_CLOCK) until it calls $finish.
#include "Vi8080_bench.h"
#include "verilated.h"

int main(int argc, char** argv) {
    VerilatedContext ctx;
    ctx.commandArgs(argc, argv);
    Vi8080_bench top{&ctx};
    while (!ctx.gotFinish()) {
        top.clock = 1;
        top.eval();
        top.clock = 0;
        top.eval();
    }
    top.final();
    return 0;
}
