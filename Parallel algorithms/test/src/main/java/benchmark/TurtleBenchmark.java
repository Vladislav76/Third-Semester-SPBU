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

import src.turtle.Movement;

import static src.general.FileReader.readFileAsPairOfIntArrays;

@Fork(1)
@Warmup(iterations = 2, time = 1)
@Measurement(iterations = 5, time = 1)
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.SECONDS)
public class TurtleBenchmark {

    @State(Scope.Thread)
    public static class State1 {

        @Setup(Level.Trial)
        public void doSetup() {
            String dirName = "src/main/input/";
            try {
                pair = readFileAsPairOfIntArrays(dirName + fileName);
                movement = new Movement();
            }
            catch (Exception e) {
                e.printStackTrace();
            }
        }

        @Param({"turtle_example_1kB", "turtle_example_64kB", "turtle_example_1MB", "turtle_example_15MB"})
        String fileName;

        @Param({"1", "2", "4", "8"})
        int threadsNumber;

        Pair<int[], int[]> pair;
        Movement movement;
    }

    @Benchmark
    public void turtle_test(State1 state) {
        state.movement.getLocation(state.pair.getKey(), state.pair.getValue(), state.threadsNumber);
    }
}
