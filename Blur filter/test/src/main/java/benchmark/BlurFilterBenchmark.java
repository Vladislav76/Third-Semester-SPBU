/*
 * Copyright (c) 2014, Oracle America, Inc.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  * Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 *  * Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 *  * Neither the name of Oracle nor the names of its contributors may be used
 *    to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 */

package benchmark;

import org.openjdk.jmh.annotations.*;

import src.Filter;
import src.Main;

import java.util.concurrent.TimeUnit;

public class BlurFilterBenchmark {

    @State(Scope.Thread)
    public static class State1 {
        @Setup(Level.Trial)
        public void doSetup() {
            String dirName = "src/main/input";
            String fileName1 = "Glasses.jpeg";
            String fileName2 = "CallOfDuty.jpg";
            String fileName3 = "Horizon.jpg";
            filter1 = new Filter();
            Main.filterSetup(dirName, fileName1, filter1);
            filter2 = new Filter();
            Main.filterSetup(dirName, fileName2, filter2);
            filter3 = new Filter();
            Main.filterSetup(dirName, fileName3, filter3);
        }
        public Filter filter1;
        public Filter filter2;
        public Filter filter3;
    }

    //---GROUP 1---

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _525x350_1_Thread_Horizontal(State1 state) {
        state.filter1.process(Filter.HORIZONTAL_PROCESSING_MODE, 1);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _525x350_2_Threads_Horizontal(State1 state) {
        state.filter1.process(Filter.HORIZONTAL_PROCESSING_MODE, 2);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _525x350_4_Threads_Horizontal(State1 state) {
        state.filter1.process(Filter.HORIZONTAL_PROCESSING_MODE, 4);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _525x350_8_Threads_Horizontal(State1 state) {
        state.filter1.process(Filter.HORIZONTAL_PROCESSING_MODE, 8);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _525x350_1_Thread_Vertical(State1 state) {
        state.filter1.process(Filter.VERTICAL_PROCESSING_MODE, 1);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _525x350_2_Threads_Vertical(State1 state) {
        state.filter1.process(Filter.VERTICAL_PROCESSING_MODE, 2);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _525x350_4_Threads_Vertical(State1 state) {
        state.filter1.process(Filter.VERTICAL_PROCESSING_MODE, 4);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _525x350_8_Threads_Vertical(State1 state) {
        state.filter1.process(Filter.VERTICAL_PROCESSING_MODE, 8);
    }

    //---GROUP 2---

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _1920x1080_1_Thread_Horizontal(State1 state) {
        state.filter2.process(Filter.HORIZONTAL_PROCESSING_MODE, 1);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _1920x1080_2_Threads_Horizontal(State1 state) {
        state.filter2.process(Filter.HORIZONTAL_PROCESSING_MODE, 2);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _1920x1080_4_Threads_Horizontal(State1 state) {
        state.filter2.process(Filter.HORIZONTAL_PROCESSING_MODE, 4);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _1920x1080_8_Threads_Horizontal(State1 state) {
        state.filter2.process(Filter.HORIZONTAL_PROCESSING_MODE, 8);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _1920x1080_1_Thread_Vertical(State1 state) {
        state.filter2.process(Filter.VERTICAL_PROCESSING_MODE, 1);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _1920x1080_2_Threads_Vertical(State1 state) {
        state.filter2.process(Filter.VERTICAL_PROCESSING_MODE, 2);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _1920x1080_4_Threads_Vertical(State1 state) {
        state.filter2.process(Filter.VERTICAL_PROCESSING_MODE, 4);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _1920x1080_8_Threads_Vertical(State1 state) {
        state.filter2.process(Filter.VERTICAL_PROCESSING_MODE, 8);
    }

    //---GROUP 3---

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _3840x2160_1_Thread_Horizontal(State1 state) {
        state.filter3.process(Filter.HORIZONTAL_PROCESSING_MODE, 1);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _3840x2160_2_Threads_Horizontal(State1 state) {
        state.filter3.process(Filter.HORIZONTAL_PROCESSING_MODE, 2);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _3840x2160_4_Threads_Horizontal(State1 state) {
        state.filter3.process(Filter.HORIZONTAL_PROCESSING_MODE, 4);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _3840x2160_8_Threads_Horizontal(State1 state) {
        state.filter3.process(Filter.HORIZONTAL_PROCESSING_MODE, 8);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _3840x2160_1_Thread_Vertical(State1 state) {
        state.filter3.process(Filter.VERTICAL_PROCESSING_MODE, 1);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _3840x2160_2_Threads_Vertical(State1 state) {
        state.filter3.process(Filter.VERTICAL_PROCESSING_MODE, 2);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _3840x2160_4_Threads_Vertical(State1 state) {
        state.filter3.process(Filter.VERTICAL_PROCESSING_MODE, 4);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _3840x2160_8_Threads_Vertical(State1 state) {
        state.filter3.process(Filter.VERTICAL_PROCESSING_MODE, 8);
    }
}
