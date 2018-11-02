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

import javafx.util.Pair;
import org.openjdk.jmh.annotations.*;
import java.util.concurrent.TimeUnit;

import src.adding_integers.BigInteger;
import src.adding_integers.Addition;

import static src.general.FileReader.readFileAsPairOfByteArrays;

public class IntegersAddingBenchmark {

    @State(Scope.Thread)
    public static class State1 {
        @Setup(Level.Trial)
        public void doSetup() {
            String fileName1 = "src/main/input/integers_example_1";
            String fileName2 = "src/main/input/integers_example_2";
            String fileName3 = "src/main/input/integers_example_3";
            String fileName4 = "src/main/input/integers_example_4";
            try {
                Pair<byte[], byte[]> pair = readFileAsPairOfByteArrays(fileName1);
                a1 = new BigInteger(pair.getKey());
                b1 = new BigInteger(pair.getValue());
                pair = readFileAsPairOfByteArrays(fileName2);
                a2 = new BigInteger(pair.getKey());
                b2 = new BigInteger(pair.getValue());
                pair = readFileAsPairOfByteArrays(fileName3);
                a3 = new BigInteger(pair.getKey());
                b3 = new BigInteger(pair.getValue());
                pair = readFileAsPairOfByteArrays(fileName4);
                a4 = new BigInteger(pair.getKey());
                b4 = new BigInteger(pair.getValue());
                addition = new Addition();
            }
            catch (Exception e) {
                e.printStackTrace();
            }
        }
        public BigInteger a1;
        public BigInteger a2;
        public BigInteger a3;
        public BigInteger a4;
        public BigInteger b1;
        public BigInteger b2;
        public BigInteger b3;
        public BigInteger b4;
        public Addition addition;
    }

    //---GROUP 1---

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _1Kb_1_Thread(State1 state) {
        state.addition.add(state.a1, state.b1, 1);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _1Kb_2_Threads(State1 state) {
        state.addition.add(state.a1, state.b1, 2);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _1Kb_4_Threads(State1 state) {
        state.addition.add(state.a1, state.b1, 4);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _1Kb_8_Threads(State1 state) {
        state.addition.add(state.a1, state.b1, 8);
    }

    //---GROUP 2---

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _64Kb_1_Thread(State1 state) {
        state.addition.add(state.a2, state.b2, 1);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _64Kb_2_Threads(State1 state) {
        state.addition.add(state.a2, state.b2, 2);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _64Kb_4_Threads(State1 state) {
        state.addition.add(state.a2, state.b2, 4);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _64Kb_8_Threads(State1 state) {
        state.addition.add(state.a2, state.b2, 8);
    }

    //---GROUP 3---

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _1Mb_1_Thread(State1 state) {
        state.addition.add(state.a3, state.b3, 1);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _1Mb_2_Threads(State1 state) {
        state.addition.add(state.a3, state.b3, 2);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _1Mb_4_Threads(State1 state) {
        state.addition.add(state.a3, state.b3, 4);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _1Mb_8_Threads(State1 state) {
        state.addition.add(state.a3, state.b3, 8);
    }

    //---GROUP 4---

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _15Mb_1_Thread(State1 state) {
        state.addition.add(state.a4, state.b4, 1);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _15Mb_2_Threads(State1 state) {
        state.addition.add(state.a4, state.b4, 2);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _15Mb_4_Threads(State1 state) {
        state.addition.add(state.a4, state.b4, 4);
    }

    @Benchmark @Fork(1) @Warmup(iterations = 2) @Measurement(iterations = 2) @BenchmarkMode(Mode.AverageTime) @OutputTimeUnit(TimeUnit.SECONDS)
    public void _15Mb_8_Threads(State1 state) {
        state.addition.add(state.a4, state.b4, 8);
    }
}
