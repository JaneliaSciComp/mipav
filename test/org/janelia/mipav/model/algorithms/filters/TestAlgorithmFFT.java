package org.janelia.mipav.model.algorithms.filters;

import java.io.IOException;

import java.util.Arrays;

import gov.nih.mipav.model.algorithms.filters.AlgorithmFFT;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.ModelStorageBase.DataType;

public class TestAlgorithmFFT {

	public static void main(String[] args) {
		float [] data = {
				0.0f,
				0.7071067811865475f,
				1.0f,
				0.7071067811865476f,
				1.2246467991473532e-16f,
				-0.7071067811865475f,
				-1.0f,
				-0.7071067811865477f
		};
		float [] data_cos = {
			  1.0f,
			  0.7071067811865476f,
			  6.123233995736766e-17f,
			 -0.7071067811865475f,
			 -1.0f,
			 -0.7071067811865477f,
			 -1.8369701987210297e-16f,
			  0.7071067811865474f,
		};
		float [] padding = new float[32];
		ModelImage img = new ModelImage(ModelStorageBase.DataType.FLOAT, new int[] { 8 }, "data");
		try {
			img.importData(0, data_cos, false);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		AlgorithmFFT fft = new AlgorithmFFT(img, AlgorithmFFT.FORWARD, false, true, false, false);
		fft.runAlgorithm();
		
		data_cos = fft.getRealData();
		try {
			img.exportComplexData(0, 8, data, data_cos);
		} catch (IOException e) {
			e.printStackTrace();
		}
		for(float f: data) {
			System.out.println(f);
		}
		Arrays.fill(padding, 0.0f);
		System.arraycopy(data, 0, padding, 12, 8);
		System.out.println("padding:");
		for(float f: padding) {
			System.out.println(f);
		}
		ModelImage padded_image = new ModelImage(DataType.FLOAT, new int[] { 32 }, "padded");
		try {
			padded_image.importData(0, padding, false);
		} catch (IOException e) {
			e.printStackTrace();
		}
		AlgorithmFFT inv_fft = new AlgorithmFFT(padded_image, AlgorithmFFT.INVERSE, false, true, false, false);
		inv_fft.runAlgorithm();
		System.out.println("Padded inverse fft:");
		for(float f: inv_fft.getRealData()) {
			System.out.println(f*4);
		}
		System.out.println(inv_fft.getRealData().length);
	}
}
