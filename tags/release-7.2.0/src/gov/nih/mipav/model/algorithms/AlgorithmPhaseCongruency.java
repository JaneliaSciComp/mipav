package gov.nih.mipav.model.algorithms;

import java.io.IOException;
import java.util.Arrays;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.filters.FFTUtility;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.ViewUserInterface;

/**
 * Ported from Peter Kovesi's MATLAB code for Phase Congruency
 * Available from: http://www.csse.uwa.edu.au/~pk/research/matlabfns/PhaseCongruency/phasecong3.m
 * and: http://www.csse.uwa.edu.au/~pk/research/matlabfns/FrequencyFilt/lowpassfilter.m
 * in reference to the method lowpassFilter();
 * 
 * % References:
%
%     Peter Kovesi, "Image Features From Phase Congruency". Videre: A
%     Journal of Computer Vision Research. MIT Press. Volume 1, Number 3,
%     Summer 1999 http://mitpress.mit.edu/e-journals/Videre/001/v13.html
%
%     Peter Kovesi, "Phase Congruency Detects Corners and
%     Edges". Proceedings DICTA 2003, Sydney Dec 10-12

% Copyright (c) 1996-2010 Peter Kovesi
% Centre for Exploration Targeting
% The University of Western Australia
% peter.kovesi at uwa edu au
% 
% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, subject to the following conditions:
% 
% The above copyright notice and this permission notice shall be included in 
% all copies or substantial portions of the Software.
%
% The Software is provided "as is", without warranty of any kind.

*/
public class AlgorithmPhaseCongruency extends AlgorithmBase {

	private int nscale;
	
	private int norient;
	
	private int minWaveLength;
	
	private double mult;
	
	private double sigmaOnf;
	
	private double k;
	
	private double cutOff;
	
	private int g;
	
	private int noiseMethod;
	
	private double epsilon = 0.0001;
	
	private int width;
	
	private int height;
	
	private int length;
	
	private double[] M;
	
	private double[] m;
	
	private double[] or;
	
	private double[] ft;
	
	private double T;
	
	public AlgorithmPhaseCongruency(ModelImage src){
		
		super(null, src);
		
		int[] extents = srcImage.getExtents();
		width = extents[0];
		height = extents[1];
		length = width * height;
		M = new double[length];
		m = new double[length];
		or = new double[length];
		ft = new double[length];
	}
	
	public void finalize(){
		super.finalize();
		M = null;
		m = null;
		or = null;
		ft = null;
	}
	
	/**% Arguments:
		%              Default values      Description
		%
		%    nscale           4    - Number of wavelet scales, try values 3-6
		%    norient          6    - Number of filter orientations.
		%    minWaveLength    3    - Wavelength of smallest scale filter.
		%    mult             2.1  - Scaling factor between successive filters.
		%    sigmaOnf         0.55 - Ratio of the standard deviation of the Gaussian 
		%                            describing the log Gabor filter's transfer function 
		%                            in the frequency domain to the filter center frequency.
		%    k                2.0  - No of standard deviations of the noise energy beyond
		%                            the mean at which we set the noise threshold point.
		%                            You may want to vary this up to a value of 10 or
		%                            20 for noisy images 
		%    cutOff           0.5  - The fractional measure of frequency spread
		%                            below which phase congruency values get penalized.
		%    g                10   - Controls the sharpness of the transition in
		%                            the sigmoid function used to weight phase
		%                            congruency for frequency spread.                        
		%    noiseMethod      -1   - Parameter specifies method used to determine
		%                            noise statistics. 
		%                              -1 use median of smallest scale filter responses
		%                              -2 use mode of smallest scale filter responses
		%                               0+ use noiseMethod value as the fixed noise threshold 
	*/
	public void setArguments(int _nscale, int _norient, int _minWaveLength, double _mult,
			double _sigmaOnf, double _k, double _cutOff, int _g, int _noiseMethod){
		nscale = _nscale;
		norient = _norient;
		minWaveLength = _minWaveLength;
		mult = _mult;
		sigmaOnf = _sigmaOnf;
		k = _k;
		cutOff = _cutOff;
		g = _g;
		noiseMethod = _noiseMethod;
	}
	
	@Override
	public void runAlgorithm() {
		
		fireProgressStateChanged(0, null, "Calculating...");
		float iterations = norient * nscale;
		
		double[] covx2 = new double[length];
		double[] covy2 = new double[length];
		double[] covxy = new double[length];
		double[] pcSum = new double[length];
		
		double[][] PC = new double[norient][length];
		double[][] energyV = new double[3][length];
		
		double[][][] EOR = new double[norient][nscale][length];
		double[][][] EOI = new double[norient][nscale][length];
		
		double[] radius = new double[length];
		double[] sintheta = new double[length];
		double[] costheta = new double[length];
		
		double[] x = new double[width];
		double[] y = new double[height];
		
		if(width % 2 == 0){
			for(int i=0;i<width;i++){
				x[i] = (double)(i - width/2)/(double)width;
			}
		} else {
			for(int i=0;i<width;i++){
				x[i] = (double)(i - (width-1)/2)/(double)(width-1);
			}
		}
		
		if(height % 2 == 0){
			for(int i=0;i<height;i++){
				y[i] = (double)(i - height/2)/(double)height;
			}
		} else {
			for(int i=0;i<height;i++){
				y[i] = (double)(i - (height-1)/2)/(double)(height-1);
			}
		}
		
		for(int j=0;j<height;j++){
			int row = j*width;
			for(int i=0;i<width;i++){
				int ind = i + row;
				radius[ind] = Math.sqrt(Math.pow(x[i], 2) + Math.pow(y[j], 2));
				double theta = Math.atan2(-y[j],x[i]);
				sintheta[ind] = Math.sin(theta);
				costheta[ind] = Math.cos(theta);
			}
		}
		
		radius = ifftShift(radius);
		sintheta = ifftShift(sintheta);
		costheta = ifftShift(costheta);
		
		radius[0] = 1;
		
		double[] lp = lowpassFilter(15);
		
		double[][] logGabor = new double[nscale][length];
		
		for(int i=0;i<nscale;i++){
			double wavelength = minWaveLength*Math.pow(mult, i);
			double fo = 1.0/wavelength;
			for(int j=0;j<length;j++){
				logGabor[i][j] = lp[j] * Math.exp(-Math.pow(Math.log(radius[j]/fo), 2)) / 
						(2.0 * Math.pow(Math.log(sigmaOnf), 2));
			}
			logGabor[i][0] = 0;
		}

		double[] imagefftR = new double[length];
		double[] imagefftI = new double[length];
		
		try {
			srcImage.exportData(0, length, imagefftR);
		} catch (IOException e1) {
			e1.printStackTrace();
		}
		
		FFTUtility fft = new FFTUtility(imagefftR, imagefftI, height, width, 1, -1, FFTUtility.FFT);
		fft.run();
		
		fft = new FFTUtility(imagefftR, imagefftI, 1, height, width, -1, FFTUtility.FFT);
		fft.run();
		
		for(int o = 0;o<norient;o++){
			double angl = (double)o * Math.PI / (double)norient;
			double[] spread = new double[length];
			for(int i=0;i<length;i++){
				double ds = sintheta[i] * Math.cos(angl) - costheta[i] * Math.sin(angl);
				double dc = costheta[i] * Math.cos(angl) + sintheta[i] * Math.sin(angl);
				double dtheta = Math.abs(Math.atan2(ds,dc));
				dtheta = Math.min(dtheta*(double)norient/2.0, Math.PI);
				spread[i] = (Math.cos(dtheta) + 1.0) / 2.0;
			}
			double[] sumE_ThisOrient = new double[length];
			double[] sumO_ThisOrient = new double[length];
			double[] sumAn_ThisOrient = new double[length];
			double[] Energy = new double[length];
			double[] MeanE = new double[length];
			double[] MeanO = new double[length];
			
			double tau = 1;
			double[] maxAn = new double[length];
			Arrays.fill(maxAn, Double.MIN_VALUE);
			
			for(int s=0;s<nscale;s++){
				double[] ifftR = new double[length];
				double[] ifftI = new double[length];
				for(int i=0;i<length;i++){
					double filter = logGabor[s][i] * spread[i];
					ifftR[i] = imagefftR[i] * filter;
					ifftI[i] = imagefftI[i] * filter;
				}
				
				FFTUtility ifft = new FFTUtility(ifftR, ifftI, height, width, 1, 1, FFTUtility.FFT);
				ifft.run();
				
				ifft = new FFTUtility(ifftR, ifftI, 1, height, width, 1, FFTUtility.FFT);
				ifft.run();
				
				EOR[o][s] = ifftR;
				EOI[o][s] = ifftI;
				
				float iterNum = 99 * (o * nscale + (s+1));
				progress = iterNum / iterations;
				fireProgressStateChanged((int)progress);
				
				
				for(int i=0;i<length;i++){
					double An = Math.sqrt(Math.pow(EOR[o][s][i], 2) + Math.pow(EOI[o][s][i], 2));
					sumAn_ThisOrient[i] += An;
					sumE_ThisOrient[i] += EOR[o][s][i];
					sumO_ThisOrient[i] += EOI[o][s][i];
					maxAn[i] = Math.max(maxAn[i], An);
				}
				
				if(s == 0){
					if(noiseMethod == -1){
						tau = findMedian(sumAn_ThisOrient)/Math.sqrt(Math.log(4));
					} else if(noiseMethod == -2){
						tau = rayleighmode(sumAn_ThisOrient);
					}
				}
			}

			for(int i=0;i<length;i++){
				energyV[0][i] += sumE_ThisOrient[i];
				energyV[1][i] += Math.cos(angl)*sumO_ThisOrient[i];
				energyV[2][i] += Math.sin(angl)*sumO_ThisOrient[i];
				double XEnergy = Math.sqrt(Math.pow(sumE_ThisOrient[i], 2)+ Math.pow(sumO_ThisOrient[i],2)) + epsilon;
				MeanE[i] = sumE_ThisOrient[i] / XEnergy;
				MeanO[i] = sumO_ThisOrient[i] / XEnergy;
				for(int s=0;s<nscale;s++){
					Energy[i] += EOR[o][s][i]*MeanE[i] + EOI[o][s][i]*MeanO[i]
							- Math.abs(EOR[o][s][i]*MeanO[i] - EOI[o][s][i]*MeanE[i]);
				}
			}

			if(noiseMethod >=0)
				T = noiseMethod;
			else{
				double totalTau = tau * (1.0 - Math.pow(1.0 / mult, nscale)) / (1.0-(1.0/mult));
				double EstNoiseEnergyMean = totalTau*Math.sqrt(Math.PI/2.0);
				double EstNoiseEnergySigma = totalTau*Math.sqrt((4.0-Math.PI)/2.0);
				T = EstNoiseEnergyMean + k*EstNoiseEnergySigma;
				ViewUserInterface.getReference().getMessageFrame().getData().append(
						"Estimated noise level for orientation " + String.valueOf(o+1) + ": " + String.valueOf(T) + "\n");
			}
			for(int i=0;i<length;i++){
				Energy[i] = Math.max(Energy[i] - T, 0);
				double width = (sumAn_ThisOrient[i]/(maxAn[i] + epsilon) - 1.0) / (double)(nscale-1.0);
				double weight = 1.0 / (1.0 + Math.exp((cutOff - width) * (double)g));
				PC[o][i] = weight * Energy[i]/sumAn_ThisOrient[i];
				pcSum[i] += PC[o][i];

				double covx = PC[o][i] * Math.cos(angl);
				double covy = PC[o][i] * Math.sin(angl);
				covx2[i] += Math.pow(covx, 2);
				covy2[i] += Math.pow(covy, 2);
				covxy[i] += covx*covy;
			}
		}
		
		for(int i=0;i<length;i++){
			covx2[i] = covx2[i]/((double)norient/2.0);
			covy2[i] = covy2[i]/((double)norient/2.0);
			covxy[i] = 4.0*covxy[i]/(double)norient;
			double denom = Math.sqrt(Math.pow(covxy[i], 2) + Math.pow(covx2[i]-covy2[i], 2)) + epsilon;
			M[i] = (covy2[i]+covx2[i] + denom)/2.0;
			m[i] = (covy2[i]+covx2[i] - denom)/2.0;
			or[i] = Math.atan2(energyV[2][i], energyV[1][i]);
			if(or[i]<0)
				or[i] += Math.PI;
			or[i] = Math.round(or[i]*180.0/Math.PI);
			
			double oddV = Math.sqrt(Math.pow(energyV[1][i],2) + Math.pow(energyV[2][i], 2));
			ft[i] = Math.atan2(energyV[0][i], oddV);
		}
		
		fireProgressStateChanged(100);
		setCompleted(true);
	}
	
	public double[] getEdges(){
		return M;
	}
	
	public double[] getCorners(){
		return m;
	}
	
	public double[] getOrientations(){
		return or;
	}
	
	public double[] getFeatureType(){
		return ft;
	}
	
	private double[] lowpassFilter(int n){
		
		double[] filter = new double[length];
		
		double[] x = new double[width];
		double[] y = new double[height];
		
		if(width % 2 == 0){
			for(int i=0;i<width;i++){
				x[i] = (double)(i - width/2)/(double)width;
			}
		} else {
			for(int i=0;i<width;i++){
				x[i] = (double)(i - (width-1)/2)/(double)(width-1);
			}
		}
		
		if(height % 2 == 0){
			for(int i=0;i<height;i++){
				y[i] = (double)(i - height/2)/(double)height;
			}
		} else {
			for(int i=0;i<height;i++){
				y[i] = (double)(i - (height-1)/2)/(double)(height-1);
			}
		}
		
		for(int j=0;j<height;j++){
			int row = j*width;
			for(int i=0;i<width;i++){
				int ind = i + row;
				double radius = Math.sqrt(Math.pow(x[i], 2) + Math.pow(y[j], 2));
				filter[ind] = 1.0 / (1.0 + Math.pow(radius / 0.45, 2*n));
			}
		}
		
		return ifftShift(filter);
		
	}
	
	private double findMedian(double[] array){
		double[] temp = new double[array.length];
		System.arraycopy(array, 0, temp, 0, array.length);
		Arrays.sort(temp);
		if(array.length % 2 == 0){
			int center = array.length / 2;
			return (temp[center-1] + temp[center])/2.0;
		} else {
			int center = (array.length - 1)/2;
			return temp[center];
		}
	}
	
	private double rayleighmode(double[] array){
		int nbins = 50;
		int[] histo = new int[nbins];
		double maxArray = Double.MIN_VALUE;
		for(int i=0;i<array.length;i++){
			maxArray = Math.max(maxArray, array[i]);
		}
		double binWidth = maxArray/(double)nbins;
		
		for(int i=0;i<array.length;i++){
			double value = array[i];
			int bin = (int) (value/binWidth);
			if(bin == nbins)
				histo[bin-1]++;
			else histo[bin]++;
		}
		
		int modeInd = 0;
		int maxFreq = Integer.MIN_VALUE;
		
		for(int i=0;i<nbins;i++){
			if(histo[i] > maxFreq){
				maxFreq = histo[i];
				modeInd = i;
			}
		}
		
		return (binWidth*(double)(2*modeInd + 1))/2.0;
	}
	
	private double[] ifftShift(double[] array){
		
		double[] shifted = new double[length];
		int stepY = height/2;
		int stepX = width/2;
		for(int j=0;j<height;j++){
			int nj = j + stepY;
			nj = nj >= height ? nj - height : nj;
			int y = j*width;
			int ny = nj*width;
			for(int i=0;i<width;i++){
				int ni = i + stepX;
				ni = ni >= width ? ni - width : ni;
				shifted[i + y] = array[ni + ny];
			}
		}
		
		return shifted;
	}


}
