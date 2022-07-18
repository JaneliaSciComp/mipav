package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework.liblinearsvm;

import static gov.nih.mipav.view.renderer.WildMagic.ProstateFramework.liblinearsvm.Linear.NL;
import static gov.nih.mipav.view.renderer.WildMagic.ProstateFramework.liblinearsvm.Linear.atof;
import static gov.nih.mipav.view.renderer.WildMagic.ProstateFramework.liblinearsvm.Linear.atoi;
import static gov.nih.mipav.view.renderer.WildMagic.ProstateFramework.liblinearsvm.Linear.closeQuietly;
import static gov.nih.mipav.view.renderer.WildMagic.ProstateFramework.liblinearsvm.Linear.printf;
import gov.nih.mipav.view.renderer.WildMagic.ProstateFramework.*;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Formatter;
import java.util.List;
import java.util.StringTokenizer;
import java.util.regex.Pattern;

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
public class Predict {

    private static boolean       flag_predict_probability = false;

    public static final Pattern COLON                    = Pattern.compile(":");

    /**
     * <p><b>Note: The streams are NOT closed</b></p>
     */
    static void doPredict(BufferedReader reader, Writer writer, Model model) throws IOException {
        int correct = 0;
        int total = 0;

        int nr_class = model.getNrClass();
        double[] prob_estimates = null;
        int n;
        int nr_feature = model.getNrFeature();
        if (model.bias >= 0)
            n = nr_feature + 1;
        else
            n = nr_feature;

        Formatter out = new Formatter(writer);

        if (flag_predict_probability) {
            if (model.solverType != SolverType.L2R_LR) {
                throw new IllegalArgumentException("probability output is only supported for logistic regression");
            }

            int[] labels = model.getLabels();
            prob_estimates = new double[nr_class];

            printf(out, "labels");
            for (int j = 0; j < nr_class; j++)
                printf(out, " %d", labels[j]);
            printf(out, "\n");
        }


        String line = null;
        while ((line = reader.readLine()) != null) {
            List<FeatureNode> x = new ArrayList<FeatureNode>();
            StringTokenizer st = new StringTokenizer(line, " \t");
            String label = st.nextToken();
            int target_label = atoi(label);

            while (st.hasMoreTokens()) {
                String[] split = COLON.split(st.nextToken(), 2);
                if (split == null || split.length < 2) exit_input_error(total + 1);

                try {
                    int idx = atoi(split[0]);
                    double val = atof(split[1]);

                    // feature indices larger than those in training are not used
                    if (idx <= nr_feature) {
                        FeatureNode node = new FeatureNode(idx, val);
                        x.add(node);
                    }
                } catch (NumberFormatException e) {
                    exit_input_error(total + 1, e);
                }
            }

            if (model.bias >= 0) {
                FeatureNode node = new FeatureNode(n, model.bias);
                x.add(node);
            }

            FeatureNode[] nodes = new FeatureNode[x.size()];
            nodes = x.toArray(nodes);

            int predict_label;

            if (flag_predict_probability) {
                predict_label = Linear.predictProbability(model, nodes, prob_estimates);
                printf(out, "%d", predict_label);
                for (int j = 0; j < model.nr_class; j++)
                    printf(out, " %g", prob_estimates[j]);
                printf(out, "\n");
            } else {
                predict_label = Linear.predict(model, nodes);
                printf(out, "%d\n", predict_label);
            }
            /*
            if (predict_label == target_label) {
                ++correct;
            }
            */
            if (predict_label == 1) {
                ++correct;
            }
            ++total;
        }
        System.out.printf("Accuracy = %g%% (%d/%d)" + NL, (double)correct / total * 100, correct, total);
    }

    /**
     * <p><b>Note: The streams are NOT closed</b></p>
     */
    /*
    public static void doPredict(int[][]classification, Features[] featureArray,  Model model, int traceingSliceNumber) {
        int correct = 0;
        int total = 0;
        int z;
        int zDim = featureArray.length;
        // z only apply to few slices ??????????????????????????????
        int nr_class = model.getNrClass();
        double[] prob_estimates = null;
        int n;
        int nr_feature = model.getNrFeature();
        if (model.bias >= 0)
            n = nr_feature + 1;
        else
            n = nr_feature;

        int midSlice = (int)(( zDim - 1 ) / 2f);
        
        for ( z = 0; z < zDim; z++ ) {
	        
        	if (z >= (midSlice - traceingSliceNumber) && z <= (midSlice + traceingSliceNumber) && z != midSlice ) {
				List<FeatureNode[]> feature = featureArray[z].getFeatureArray();
				List<Integer> classify = featureArray[z].getClassArray();

				int len = feature.size();

				for (int i = 0; i < len; i++) {
					// List<FeatureNode> x = new ArrayList<FeatureNode>();
					int target_label = classify.get(i);
					FeatureNode[] nodes = feature.get(i);
					// nodes = x.toArray(nodes);

					int predict_label;
					predict_label = Linear.predict(model, nodes);

					classification[z][i] = predict_label;

					if (predict_label == target_label) {
						++correct;
					}
					++total;
				}
				System.out.printf("Accuracy = %g%% (%d/%d)" + NL,
						(double) correct / total * 100, correct, total);
			}
        }
    }
   */
    
    public static void doPredict(int[][]classification, Features[] featureArray,  Model model, int currentSlice) {
    int correct = 0;
    int total = 0;
    int z;
    int zDim = featureArray.length;
        int nr_class = model.getNrClass();
        double[] prob_estimates = null;
        int n;
        int nr_feature = model.getNrFeature();
        if (model.bias >= 0)
            n = nr_feature + 1;
        else
            n = nr_feature;
   
        z = currentSlice;
	        
		List<FeatureNode[]> feature = featureArray[z].getFeatureArray();
		List<Integer> classify = featureArray[z].getClassArray();

		int len = feature.size();

		for (int i = 0; i < len; i++) {
			// List<FeatureNode> x = new ArrayList<FeatureNode>();
			int target_label = classify.get(i);
			FeatureNode[] nodes = feature.get(i);
			// nodes = x.toArray(nodes);

			int predict_label;
			predict_label = Linear.predict(model, nodes);

			classification[z][i] = predict_label;

			if (predict_label == target_label) {
				++correct;
			}
			++total;
		}
		// System.out.printf("Accuracy = %g%% (%d/%d)" + NL, (double) correct / total * 100, correct, total);
		
    
    }

    public static double doPredict(int[][]classification, Features[] featureArray,  Model model, int currentSlice, int index) {
        int correct = 0;
        int total = 0;
        // int z;
        int zDim = featureArray.length;
            int nr_class = model.getNrClass();
            double[] prob_estimates = null;
            int n;
            int nr_feature = model.getNrFeature();
            if (model.bias >= 0)
                n = nr_feature + 1;
            else
                n = nr_feature;
       
            // z = currentSlice;
    	        
    		List<FeatureNode[]> feature = featureArray[currentSlice].getFeatureArray();
    		List<Integer> classify = featureArray[currentSlice].getClassArray();

    		int len = feature.size();

    		for (int i = 0; i < len; i++) {
    			// List<FeatureNode> x = new ArrayList<FeatureNode>();
    			int target_label = classify.get(i);
    			FeatureNode[] nodes = feature.get(i);
    			// nodes = x.toArray(nodes);

    			int predict_label;
    			predict_label = Linear.predict(model, nodes);

    			classification[index][i] = predict_label;

    			if (predict_label == 1) {
    				++correct;
    			}
    			++total;
    		}
    		// System.out.printf("Accuracy = %g%% (%d/%d)" + NL, (double) correct / total * 100, correct, total);
    		return ((double)correct/total*100);
        
        }

    
    private static void exit_input_error(int line_num, Throwable cause) {
        throw new RuntimeException("Wrong input format at line " + line_num, cause);
    }

    private static void exit_input_error(int line_num) {
        throw new RuntimeException("Wrong input format at line " + line_num);
    }

    private static void exit_with_help() {
        System.out.println("Usage: predict [options] test_file model_file output_file" + NL //
            + "options:" + NL //
            + "-b probability_estimates: whether to output probability estimates, 0 or 1 (default 0)" + NL //
        );
        System.exit(1);
    }
    /*
    public static void main(String[] argv) throws IOException {
        int i;

        // parse options
        for (i = 0; i < argv.length; i++) {
            if (argv[i].charAt(0) != '-') break;
            ++i;
            switch (argv[i - 1].charAt(1)) {
                case 'b':
                    try {
                        flag_predict_probability = (atoi(argv[i]) != 0);
                    } catch (NumberFormatException e) {
                        exit_with_help();
                    }
                    break;

                default:
                    System.err.println("unknown option: -" + argv[i - 1].charAt(1) + NL);
                    exit_with_help();
                    break;
            }
        }
        if (i >= argv.length || argv.length <= i + 2) {
            exit_with_help();
        }

        BufferedReader reader = null;
        Writer writer = null;
        try {
            reader = new BufferedReader(new InputStreamReader(new FileInputStream(argv[i]), Linear.FILE_CHARSET));
            writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(argv[i + 2]), Linear.FILE_CHARSET));

            Model model = Linear.loadModel(new File(argv[i + 1]));
            doPredict(reader, writer, model);
        }
        finally {
            closeQuietly(reader);
            closeQuietly(writer);
        }
    }
    */
    public void run(String[] argv) throws IOException {
        int i;

        // parse options
        for (i = 0; i < argv.length; i++) {
            if (argv[i].charAt(0) != '-') break;
            ++i;
            switch (argv[i - 1].charAt(1)) {
                case 'b':
                    try {
                        flag_predict_probability = (atoi(argv[i]) != 0);
                    } catch (NumberFormatException e) {
                        exit_with_help();
                    }
                    break;

                default:
                    System.err.println("unknown option: -" + argv[i - 1].charAt(1) + NL);
                    exit_with_help();
                    break;
            }
        }
        if (i >= argv.length || argv.length <= i + 2) {
            exit_with_help();
        }

        BufferedReader reader = null;
        Writer writer = null;
        try {
            reader = new BufferedReader(new InputStreamReader(new FileInputStream(argv[i]), Linear.FILE_CHARSET));
            writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(argv[i + 2]), Linear.FILE_CHARSET));

            Model model = Linear.loadModel(new File(argv[i + 1]));
            System.err.println(model.toString());
            doPredict(reader, writer, model);
        }
        finally {
            closeQuietly(reader);
            closeQuietly(writer);
        }
    }
}
