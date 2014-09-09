package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework.liblinearsvm;

import static gov.nih.mipav.view.renderer.WildMagic.ProstateFramework.liblinearsvm.Linear.NL;
import static gov.nih.mipav.view.renderer.WildMagic.ProstateFramework.liblinearsvm.Linear.atof;
import static gov.nih.mipav.view.renderer.WildMagic.ProstateFramework.liblinearsvm.Linear.atoi;

import gov.nih.mipav.view.renderer.WildMagic.ProstateFramework.Features;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

/**
 * 
 * Copyright (c) 2007-2014 The LIBLINEAR Project.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. Neither name of copyright holders nor the names of its contributors
 * may be used to endorse or promote products derived from this software
 * without specific prior written permission.
 * 
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 */
public class Train {

    public static void main(String[] args) throws IOException, InvalidInputDataException {
        new Train().run(args);
    }

    private double    bias             = 1;
    private boolean   cross_validation = false;
    private String    inputFilename;
    private String    modelFilename;
    private int       nr_fold;
    private Parameter param            = null;
    private Problem   prob             = null;
    private Model model;
    
    private void do_cross_validation() {
        int[] target = new int[prob.l];

        long start, stop;
        start = System.currentTimeMillis();
        Linear.crossValidation(prob, param, nr_fold, target);
        stop = System.currentTimeMillis();
        System.out.println("time: " + (stop - start) + " ms");

        int total_correct = 0;
        for (int i = 0; i < prob.l; i++)
            if (target[i] == prob.y[i]) ++total_correct;

        System.out.printf("correct: %d" + NL, total_correct);
        System.out.printf("Cross Validation Accuracy = %g%%\n", 100.0 * total_correct / prob.l);
    }

    private void exit_with_help() {
        System.out.println("Usage: train [options] training_set_file [model_file]" + NL //
            + "options:" + NL//
            + "-s type : set type of solver (default 1)" + NL//
            + "   0 -- L2-regularized logistic regression" + NL//
            + "   1 -- L2-regularized L2-loss support vector classification (dual)" + NL//
            + "   2 -- L2-regularized L2-loss support vector classification (primal)" + NL//
            + "   3 -- L2-regularized L1-loss support vector classification (dual)" + NL//
            + "   4 -- multi-class support vector classification by Crammer and Singer" + NL//
            + "   5 -- L1-regularized L2-loss support vector classification" + NL//
            + "   6 -- L1-regularized logistic regression" + NL//
            + "-c cost : set the parameter C (default 1)" + NL//
            + "-e epsilon : set tolerance of termination criterion" + NL//
            + "   -s 0 and 2" + NL//
            + "       |f'(w)|_2 <= eps*min(pos,neg)/l*|f'(w0)|_2," + NL//
            + "       where f is the primal function and pos/neg are # of" + NL//
            + "       positive/negative data (default 0.01)" + NL//
            + "   -s 1, 3, and 4" + NL//
            + "       Dual maximal violation <= eps; similar to libsvm (default 0.1)" + NL//
            + "   -s 5 and 6" + NL//
            + "       |f'(w)|_inf <= eps*min(pos,neg)/l*|f'(w0)|_inf," + NL//
            + "       where f is the primal function (default 0.01)" + NL//
            + "-B bias : if bias >= 0, instance x becomes [x; bias]; if < 0, no bias term added (default -1)" + NL//
            + "-wi weight: weights adjust the parameter C of different classes (see README for details)" + NL//
            + "-v n: n-fold cross validation mode" + NL//
            + "-q : quiet mode (no outputs)" + NL//
        );
        System.exit(1);
    }


    Problem getProblem() {
        return prob;
    }

    double getBias() {
        return bias;
    }

    Parameter getParameter() {
        return param;
    }

    void parse_command_line(String argv[]) {
        int i;

        // eps: see setting below
        param = new Parameter(SolverType.L2R_L2LOSS_SVC_DUAL, 1, Double.POSITIVE_INFINITY);
        // default values
        bias = -1;
        cross_validation = false;

        int nr_weight = 0;

        // parse options
        for (i = 0; i < argv.length; i++) {
            if (argv[i].charAt(0) != '-') break;
            if (++i >= argv.length) exit_with_help();
            switch (argv[i - 1].charAt(1)) {
                case 's':
                    param.solverType = SolverType.values()[atoi(argv[i])];
                    break;
                case 'c':
                    param.setC(atof(argv[i]));
                    break;
                case 'e':
                    param.setEps(atof(argv[i]));
                    break;
                case 'B':
                    bias = atof(argv[i]);
                    break;
                case 'w':
                    ++nr_weight;
                    {
                        int[] old = param.weightLabel;
                        param.weightLabel = new int[nr_weight];
                        System.arraycopy(old, 0, param.weightLabel, 0, nr_weight - 1);
                    }

                    {
                        double[] old = param.weight;
                        param.weight = new double[nr_weight];
                        System.arraycopy(old, 0, param.weight, 0, nr_weight - 1);
                    }

                    param.weightLabel[nr_weight - 1] = atoi(argv[i - 1].substring(2));
                    param.weight[nr_weight - 1] = atof(argv[i]);
                    break;
                case 'v':
                    cross_validation = true;
                    nr_fold = atoi(argv[i]);
                    if (nr_fold < 2) {
                        System.err.print("n-fold cross validation: n must >= 2\n");
                        exit_with_help();
                    }
                    break;
                case 'q':
                    Linear.disableDebugOutput();
                    break;
                default:
                    System.err.println("unknown option");
                    exit_with_help();
            }
        }

        // determine filenames

        if (i >= argv.length) exit_with_help();

        inputFilename = argv[i];

        if (i < argv.length - 1)
            modelFilename = argv[i + 1];
        else {
            int p = argv[i].lastIndexOf('/');
            ++p; // whew...
            modelFilename = argv[i].substring(p) + ".model";
        }

        if (param.eps == Double.POSITIVE_INFINITY) {
            if (param.solverType == SolverType.L2R_LR || param.solverType == SolverType.L2R_L2LOSS_SVC) {
                param.setEps(0.01);
            } else if (param.solverType == SolverType.L2R_L2LOSS_SVC_DUAL || param.solverType == SolverType.L2R_L1LOSS_SVC_DUAL
                || param.solverType == SolverType.MCSVM_CS) {
                param.setEps(0.1);
            } else if (param.solverType == SolverType.L1R_L2LOSS_SVC || param.solverType == SolverType.L1R_LR) {
                param.setEps(0.01);
            }
        }
    }

    private void setParameters() {
        int i;

        // eps: see setting below
        param = new Parameter(SolverType.L2R_L2LOSS_SVC_DUAL, 1, Double.POSITIVE_INFINITY);
        // default values
        bias = -1;
        cross_validation = false;

        int nr_weight = 0;

       
        param.setEps(0.01f);
        param.solverType = SolverType.values()[3];
        
        if (param.eps == Double.POSITIVE_INFINITY) {
            if (param.solverType == SolverType.L2R_LR || param.solverType == SolverType.L2R_L2LOSS_SVC) {
                param.setEps(0.01);
            } else if (param.solverType == SolverType.L2R_L2LOSS_SVC_DUAL || param.solverType == SolverType.L2R_L1LOSS_SVC_DUAL
                || param.solverType == SolverType.MCSVM_CS) {
                param.setEps(0.1);
            } else if (param.solverType == SolverType.L1R_L2LOSS_SVC || param.solverType == SolverType.L1R_LR) {
                param.setEps(0.01);
            }
        }
    }

    
    /**
     * reads a problem from LibSVM format
     * @param filename the name of the svm file
     * @throws IOException obviously in case of any I/O exception ;)
     * @throws InvalidInputDataException if the input file is not correctly formatted
     */
    public static Problem readProblem(File file, double bias) throws IOException, InvalidInputDataException {
        BufferedReader fp = new BufferedReader(new FileReader(file));
        List<Integer> vy = new ArrayList<Integer>();
        List<FeatureNode[]> vx = new ArrayList<FeatureNode[]>();
        int max_index = 0;

        int lineNr = 0;

        try {
            while (true) {
                String line = fp.readLine();
                if (line == null) break;
                lineNr++;

                StringTokenizer st = new StringTokenizer(line, " \t\n\r\f:");
                String token = st.nextToken();

                try {
                    vy.add(atoi(token));
                } catch (NumberFormatException e) {
                    throw new InvalidInputDataException("invalid label: " + token, file, lineNr, e);
                }

                int m = st.countTokens() / 2;
                FeatureNode[] x;
                if (bias >= 0) {
                    x = new FeatureNode[m + 1];
                } else {
                    x = new FeatureNode[m];
                }
                int indexBefore = 0;
                for (int j = 0; j < m; j++) {

                    token = st.nextToken();
                    int index;
                    try {
                        index = atoi(token);
                    } catch (NumberFormatException e) {
                        throw new InvalidInputDataException("invalid index: " + token, file, lineNr, e);
                    }

                    // assert that indices are valid and sorted
                    if (index < 0) throw new InvalidInputDataException("invalid index: " + index, file, lineNr);
                    if (index <= indexBefore) throw new InvalidInputDataException("indices must be sorted in ascending order", file, lineNr);
                    indexBefore = index;

                    token = st.nextToken();
                    try {
                        double value = atof(token);
                        x[j] = new FeatureNode(index, value);
                    } catch (NumberFormatException e) {
                        throw new InvalidInputDataException("invalid value: " + token, file, lineNr);
                    }
                }
                if (m > 0) {
                    max_index = Math.max(max_index, x[m - 1].index);
                }

                vx.add(x);
            }
            System.err.println("max_index = " + max_index);
            return constructProblem(vy, vx, max_index, bias);
        } catch ( IOException e ) {
        	e.printStackTrace();
            return null;
        } catch ( InvalidInputDataException ie ) {
        	ie.printStackTrace();
        	return null;
        }
        finally {
            fp.close();
        }
    }

    void readProblem(String filename) throws IOException, InvalidInputDataException {
        prob = Train.readProblem(new File(filename), bias);
    }

    private static Problem constructProblem(List<Integer> vy, List<FeatureNode[]> vx, int max_index, double bias) {
        Problem prob = new Problem();
        prob.bias = bias;
        prob.l = vy.size();
        prob.n = max_index;
        if (bias >= 0) {
            prob.n++;
        }
        prob.x = new FeatureNode[prob.l][];
        for (int i = 0; i < prob.l; i++) {
            prob.x[i] = vx.get(i);

            if (bias >= 0) {
                assert prob.x[i][prob.x[i].length - 1] == null;
                prob.x[i][prob.x[i].length - 1] = new FeatureNode(max_index + 1, bias);
            } else {
                assert prob.x[i][prob.x[i].length - 1] != null;
            }
        }

        prob.y = new int[prob.l];
        for (int i = 0; i < prob.l; i++)
            prob.y[i] = vy.get(i);

        return prob;
    }

    public void run(String[] args) throws IOException, InvalidInputDataException {
        parse_command_line(args);
        readProblem(inputFilename);
        if (cross_validation)
            do_cross_validation();
        else {
            Model model = Linear.train(prob, param);
            Linear.saveModel(new File(modelFilename), model);
        }
    }
    
    public void run(Features[] featureArray, int numberFeatures) {
        setParameters();
        // readProblem(inputFilename);
        int middleSlice = featureArray.length / 2 - 1 ;
        prob = constructProblem(featureArray[middleSlice].getClassArray(), featureArray[middleSlice].getFeatureArray(), numberFeatures, bias);
        if (cross_validation)
            do_cross_validation();
        else {
            model = Linear.train(prob, param);
            // Linear.saveModel(new File(modelFilename), model);
        }
    }
    
    public void run(List<Integer> classes, List<FeatureNode[]> feature, int numberFeatures) {
        setParameters();
        prob = constructProblem(classes, feature, numberFeatures, bias);
        if (cross_validation)
            do_cross_validation();
        else {
            model = Linear.train(prob, param);
            // Linear.saveModel(new File(modelFilename), model);
        }
    }
    
    public Model getModel() {
    	return model;
    }
    
}
