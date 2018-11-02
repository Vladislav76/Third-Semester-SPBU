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
import java.util.concurrent.TimeUnit;

import src.parentheses.Analysis;

import static src.general.FileReader.readFileAsString;

public class ParenthesesBenchmark {

    @State(Scope.Thread)
    public static class State1 {
        @Setup(Level.Trial)
        public void doSetup() {
            String fileName1 = "src/main/input/parentheses_example_1";
            String fileName2 = "src/main/input/parentheses_example_2";
            String fileName3 = "src/main/input/parentheses_example_3";
            String fileName4 = "src/main/input/parentheses_example_4";
            try {
                s1 = readFileAsString(fileName1);
                s2 = readFileAsString(fileName2);
                s3 = readFileAsString(fileName3);
                s4 = readFileAsString(fileName4);
                analysis = new Analysis();
            }
            catch (Exception e) {
                e.printStackTrace();
            }
        }
        public String s1;
        public String s2;
        public String s3;
        public String s4;
        public Analysis analysis;
    }

    //---GROUP 1---

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _1Kb_1_Thread(State1 state) {
        state.analysis.isCorrect(state.s1, 1);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _1Kb_2_Threads(State1 state) {
        state.analysis.isCorrect(state.s1, 2);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _1Kb_4_Threads(State1 state) {
        state.analysis.isCorrect(state.s1, 4);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _1Kb_8_Threads(State1 state) {
        state.analysis.isCorrect(state.s1, 8);
    }

    //---GROUP 2---

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _64Kb_1_Thread(State1 state) {
        state.analysis.isCorrect(state.s2, 1);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _64Kb_2_Threads(State1 state) {
        state.analysis.isCorrect(state.s2, 2);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _64Kb_4_Threads(State1 state) {
        state.analysis.isCorrect(state.s2, 4);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _64Kb_8_Threads(State1 state) {
        state.analysis.isCorrect(state.s2, 8);
    }

    //---GROUP 3---

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _1Mb_1_Thread(State1 state) {
        state.analysis.isCorrect(state.s3, 1);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _1Mb_2_Threads(State1 state) {
        state.analysis.isCorrect(state.s3, 2);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _1Mb_4_Threads(State1 state) {
        state.analysis.isCorrect(state.s3, 4);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _1Mb_8_Threads(State1 state) {
        state.analysis.isCorrect(state.s3, 8);
    }

    //---GROUP 4---

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _15Mb_1_Thread(State1 state) {
        state.analysis.isCorrect(state.s4, 1);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _15Mb_2_Threads(State1 state) {
        state.analysis.isCorrect(state.s4, 2);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _15Mb_4_Threads(State1 state) {
        state.analysis.isCorrect(state.s4, 4);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _15Mb_8_Threads(State1 state) {
        state.analysis.isCorrect(state.s4, 8);
    }
}
