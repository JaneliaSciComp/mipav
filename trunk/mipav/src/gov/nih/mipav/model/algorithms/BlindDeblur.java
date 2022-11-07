package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.CeresSolver.Pair;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;
import java.util.Vector;
import java.util.logging.FileHandler;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

/**
 * 
 * @author ilb Ported to Java by William Gandler
 * Title:      Matlab Code for Fast Blind Removal of Non-Uniform Camera Shake Blur
Author:     Oliver Whyte <oawhyte@gmail.com>
Version:    1.0
Date:       Sept 17, 2014
Copyright:  2012, Oliver Whyte
URL:        http://www.di.ens.fr/willow/research/deblurring/, 
            http://www.di.ens.fr/willow/research/saturation/

Matlab Code for Fast Blind Removal of Non-Uniform Camera Shake Blur
===================================================================

This package contains code to perform fast blind deblurring of images degraded 
by camera shake, using the MAP algorithm described in our IJCV 2012 paper 
[][#Whyte12], and the fast approximation of spatially-varying blur described in 
our CPCV 2011 paper [][#Whyte11]. Please cite these papers if using this code 
in an academic publication.

Please send bug reports to <oawhyte@gmail.com>

[#Whyte11]: O. Whyte, J. Sivic, and A. Zisserman. "Deblurring Shaken and
Partially Saturated Images". In Proc. CPCV Workshop, with ICCV, 2011.

[#Whyte12]: O. Whyte, J. Sivic, A. Zisserman, and J. Ponce. "Non-uniform 
Deblurring for Shaken Images". IJCV, 2011 (accepted).

## 1 Acknowledgements ##

This package includes modified versions of code published by several other authors:

* Sparse Deconvolution code of Levin et al. Downloaded from <http://groups.csail.mit.edu/graphics/CodedAperture/DeconvolutionCode.html>
* Fast Deconvolution code of Krishnan and Fergus. Downloaded from <http://cs.nyu.edu/~dilip/research/fast-deconvolution/>
* FFT-based Poisson image blending by Amit Agrawal. Downloaded May 2008 from <http://www.umiacs.umd.edu/~aagrawal/ICCV2007Course/PseudoCode.PDF>
* Shock-filter code of Guy Gilboa. Downloaded from <http://visl.technion.ac.il/~gilboa/PDE-filt/shock.m>
* LARS-LASSO code of Karl Skoglund. Most recent version available at <http://www.imm.dtu.dk/projects/spasm/>

## 3 License ##

Copyright (c) 2012, Oliver Whyte

Permission is hereby granted, free of charge, to any person obtaining a copy of 
this software and associated documentation files (the "Software"), to deal in 
the Software without restriction, including without limitation the rights to 
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies 
of the Software, and to permit persons to whom the Software is furnished to do 
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all 
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE 
SOFTWARE.
 */

public class BlindDeblur extends AlgorithmBase {
	//private String logFile = "C:/deblur_fast_nonuniform/DeblurLogFile.log";
	private String logFile;
	private int max_dim; 
	private boolean synthetic; // If true, generate blurred image from true image
	private String results_dir;
	private String blurredImageFileDirectory;
	// File name of image to deblur
	// file_shake
	private String blurredImageFileName;
	private String trueImageFileDirectory;
	private String trueImageFileName;
	private String trueImageExtension;
	private boolean israw_true;
	private double focalLengthTrue = Double.NaN;
	private double focalLength35True = Double.NaN;
	private String cameraMakeTrue = null;
	private String cameraModelTrue = null;
	private double exposureTimeTrue = Double.NaN;
	private double FNumberTrue = Double.NaN;
	private double ISOSpeedRatingTrue = Double.NaN;
	private ModelImage blurredImage;
	private ModelImage correctedImage;
	private ModelImage trueImage;
	private String config_name;
	private boolean do_display;
	private int save_intermediate_images;
	private boolean save_mat;
	private boolean save_intermediate_pyramids;
	private boolean israw;
	private double bi_sigma_spatial0;
	private double bi_sigma_range0;
	private int bi_size;
	private int shock_dt0;
	private int shock_iters;
	private double param_decrease;
	private int grad_dir_bins;
	private double grad_dir_quant;
	private double grad_thresh_decrease;
	private int r;
	private boolean non_uniform;
	private int num_vert_regions;
	private int num_horz_regions;
	private double omega0;
	private double omega1;
	private double omega2;
	private double alpha;
	private double kf_lambda;
	private double kf_exponent;
	private double kernel_threshold;
	private double beta;
	private int num_cg_iters;
	private double theta_pre[] = new double[3];
	private int pixels_per_theta_step;
	private int BLUR_KERNEL_SIZE; // For deblur_book 151, deblur_garden 150
	                              // deblur_istanbul 55, deblur_livingroom 95
	                              // deblur_notredame 145
	private int blur_x_lims[] = new int[2];
	private int blur_y_lims[] = new int[2];
	private int blur_z_lims[] = new int[2];
	private double scale_ratio_i;
	private double scale_ratio_k;
	private int max_levels;
	private int num_iters[];
	private int recenter_kernel;
	private int kernel_dilate_radius;
	private double sat_thresh;
	private String kernel_method;
	private String image_method;
	private String image_method_final; // deblur_book "krishnan"
	private boolean do_estimate_kernel;
	private boolean do_deblur[];
	private boolean do_color[];
	private String estimate_kernel_from;
	private int deconv_maxit;
	private boolean threshold_kernel;
	private double focal_length_in_35mm_true;
	private double focal_length_in_35mm_shake;
	private int first_level;
	private int final_level;
	private String BtB_method;
	private int max_nonzeros_w;
	private boolean update_saturation_mask;
	private boolean fast_approx;
	private Vector<Pair<String,Double>>cameraModelList = null;
	
	
	    public BlindDeblur() {
	    }
	    
	    public BlindDeblur(ModelImage correctedImage, ModelImage blurredImage, 
	    		ModelImage trueImage, String results_dir, 
	    		String logFile, int BLUR_KERNEL_SIZE) {
	    	super(correctedImage, blurredImage);
	    	this.trueImage = trueImage;
	    	this.results_dir = results_dir;
	    	this.logFile = logFile;
	    	this.BLUR_KERNEL_SIZE = BLUR_KERNEL_SIZE;
	    }
	    
	    class Pair<T,U>
		{
		    public T first;
		    public U second;

		    public Pair(T t, U u)
		    {
		        first = t;
		        second = u;
		    }
		    
		    public T getFirst()
		    {
		        return first; 
		    }

		    public U getSecond()
		    {
		        return second; 
		    }

		    
		    public boolean equals(Object obj) {
		      if (obj == null) return false;
		      if ((obj.getClass() != this.getClass())) { //|| (obj.hashCode() != this.hashCode())) {
		        return false;
		      }
		      
		      return (this.getFirst().equals(((Pair) obj).getFirst()) && this.getSecond().equals(((Pair) obj).getSecond()));
		    }
		    
		    /**
		     * Define a hash code based on the first and second's hash code
		     */
		    public int hashCode() {
		      return first.hashCode() ^ second.hashCode();
		    }
		    
		    public String toString() {
		      return "Pair(" + first + ", " + second + ")";
		    }
		  
		} 
	    
	   private void initCameraModelList() {
	       cameraModelList = new Vector<Pair<String,Double>>();   
	       cameraModelList.add(new Pair("appleiphone3gs",3.20));
	       cameraModelList.add(new Pair("appleiphone",3.20));
	       cameraModelList.add(new Pair("appleiphone4",4.592));
	       cameraModelList.add(new Pair("asahiopticalco.,ltd.pentaxoptio330rs",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("canoncanondigitalixus30",5.76));
	       cameraModelList.add(new Pair("canoncanondigitalixus300",5.312));
	       cameraModelList.add(new Pair("canoncanondigitalixus40",5.76/*#1/2.5*/));
	       cameraModelList.add(new Pair("canoncanondigitalixus400",7.176/*#1/1.8*/));
	       cameraModelList.add(new Pair("canoncanondigitalixus430",7.176/*1/1.8"*/));
	       cameraModelList.add(new Pair("canoncanondigitalixus50",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("canoncanondigitalixus500",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("canoncanondigitalixus55",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("canoncanondigitalixus60",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("canoncanondigitalixus65",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("canoncanondigitalixus70",5.75));
	       cameraModelList.add(new Pair("canoncanondigitalixus700",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("canoncanondigitalixus75",5.75));
	       cameraModelList.add(new Pair("canoncanondigitalixus750",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("canoncanondigitalixus800is",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("canoncanondigitalixus860is",5.75));
	       cameraModelList.add(new Pair("canoncanondigitalixus90is",6.16));
	       cameraModelList.add(new Pair("canoncanondigitalixus900ti",7.144));
	       cameraModelList.add(new Pair("canoncanondigitalixus95is",6.16/*1/2.3*/));
	       cameraModelList.add(new Pair("canoncanondigitalixus950is",5.75/*1/2.5*/));
	       cameraModelList.add(new Pair("canoncanondigitalixus980is",7.6/*1/1.7*/));
	       cameraModelList.add(new Pair("canoncanondigitalixusi",5.744));
	       cameraModelList.add(new Pair("canoncanondigitalixusii",5.27/*1/2.7*/));
	       cameraModelList.add(new Pair("canoncanondigitalixusv",5.27));
	       cameraModelList.add(new Pair("canoncanondigitalixusv2",5.27));
	       cameraModelList.add(new Pair("canoncanondigitalixusv3",5.27));
	       cameraModelList.add(new Pair("canoncanondigitalixus",5.27));
	       cameraModelList.add(new Pair("canoncanoneos20d",22.5));
	       cameraModelList.add(new Pair("canoncanoneos1000d",22.2));
	       cameraModelList.add(new Pair("canoncanoneos10d",22.7));
	       cameraModelList.add(new Pair("canoncanoneos20d",22.5));
	       cameraModelList.add(new Pair("canoncanoneos300ddigital",22.66));
	       cameraModelList.add(new Pair("canoncanoneos30d",22.5));
	       cameraModelList.add(new Pair("canoncanoneos350ddigital",22.2));
	       cameraModelList.add(new Pair("canoncanoneos400ddigital",22.2));
	       cameraModelList.add(new Pair("canoncanoneos40d",22.2));
	       cameraModelList.add(new Pair("canoncanoneos450d",22.2));
	       cameraModelList.add(new Pair("canoncanoneos500d",22.3));
	       cameraModelList.add(new Pair("canoncanoneos50d",22.3));
	       cameraModelList.add(new Pair("canoncanoneos5dmarkii",36));
	       cameraModelList.add(new Pair("canoncanoneos5d",35.8));
	       cameraModelList.add(new Pair("canoncanoneos7d",22.3));
	       cameraModelList.add(new Pair("canoncanoneosd60",22.7));
	       cameraModelList.add(new Pair("canoncanoneosdigitalrebelxs",22.2));
	       cameraModelList.add(new Pair("canoncanoneosdigitalrebelxsi",22.2));
	       cameraModelList.add(new Pair("canoncanoneosdigitalrebelxt",22.2));
	       cameraModelList.add(new Pair("canoncanoneosdigitalrebelxti",22.2));
	       cameraModelList.add(new Pair("canoncanoneosdigitalrebel",22.66));
	       cameraModelList.add(new Pair("canoncanoneoskissdigitaln",22.2));
	       cameraModelList.add(new Pair("canoncanoneoskissdigital",22.66));
	       cameraModelList.add(new Pair("canoncanoneosrebelt1i",22.3));
	       cameraModelList.add(new Pair("canoncanoneos-1dmarkii",28.7));
	       cameraModelList.add(new Pair("canoncanoneos-1dmarkiii",28.7));
	       cameraModelList.add(new Pair("canoncanoneos-1dsmarkii",35.95));
	       cameraModelList.add(new Pair("canoncanonixus210",6.16/*1/2.3*/));
	       cameraModelList.add(new Pair("canoncanonixydigital55",5.75));
	       cameraModelList.add(new Pair("canoncanonixydigital600",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("canoncanonpowershota10",5.27));
	       cameraModelList.add(new Pair("canoncanonpowershota20",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("canoncanonpowershota2000is",6.17));
	       cameraModelList.add(new Pair("canoncanonpowershota40",5.27/*1/2.7*/));
	       cameraModelList.add(new Pair("canoncanonpowershota400",4.54/*1/3.2*/));
	       cameraModelList.add(new Pair("canoncanonpowershota510",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("canoncanonpowershota520",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("canoncanonpowershota530",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("canoncanonpowershota540",5.75));
	       cameraModelList.add(new Pair("canoncanonpowershota550",5.75/*1/2.5*/));
	       cameraModelList.add(new Pair("canoncanonpowershota560",5.75));
	       cameraModelList.add(new Pair("canoncanonpowershota60",5.27/*1/2.7*/));
	       cameraModelList.add(new Pair("canoncanonpowershota610",7.18));
	       cameraModelList.add(new Pair("canoncanonpowershota620",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("canoncanonpowershota630",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("canoncanonpowershota640",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("canoncanonpowershota650is",7.60));
	       cameraModelList.add(new Pair("canoncanonpowershota70",5.27/*1/2.7*/));
	       cameraModelList.add(new Pair("canoncanonpowershota700",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("canoncanonpowershota710is",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("canoncanonpowershota75",5.27/*1/2.7*/));
	       cameraModelList.add(new Pair("canoncanonpowershota80",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("canoncanonpowershota85",5.27/*1/2.7*/));
	       cameraModelList.add(new Pair("canoncanonpowershota95",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("canoncanonpowershotg1",7.176/*1/1.8"*/));
	       cameraModelList.add(new Pair("canoncanonpowershotg11",7.60));
	       cameraModelList.add(new Pair("canoncanonpowershotg2",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("canoncanonpowershotg3",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("canoncanonpowershotg5",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("canoncanonpowershotg6",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("canoncanonpowershotg7",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("canoncanonpowershotg9",7.600/*1/1.7*/));
	       cameraModelList.add(new Pair("canoncanonpowershotpro1",8.8/*2/3*/));
	       cameraModelList.add(new Pair("canoncanonpowershots1is",5.27/*1/2.7*/));
	       cameraModelList.add(new Pair("canoncanonpowershots110",5.27/*1/2.7*/));
	       cameraModelList.add(new Pair("canoncanonpowershots2is",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("canoncanonpowershots20",7.18));
	       cameraModelList.add(new Pair("canoncanonpowershots200",5.27/*1/2.7*/));
	       cameraModelList.add(new Pair("canoncanonpowershots3is",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("canoncanonpowershots30",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("canoncanonpowershots40",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("canoncanonpowershots400",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("canoncanonpowershots410",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("canoncanonpowershots45",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("canoncanonpowershots5is",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("canoncanonpowershots50",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("canoncanonpowershots500",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("canoncanonpowershots60",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("canoncanonpowershots70",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("canoncanonpowershots80",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("canoncanonpowershots90",7.49));
	       cameraModelList.add(new Pair("canoncanonpowershotsd10",5.75/*1/2.5*/));
	       cameraModelList.add(new Pair("canoncanonpowershotsd100",5.27/*1/2.7*/));
	       cameraModelList.add(new Pair("canoncanonpowershotsd1000",5.75/*1/2.5*/));
	       cameraModelList.add(new Pair("canoncanonpowershotsd110",5.27/*1/2.7*/));
	       cameraModelList.add(new Pair("canoncanonpowershotsd1100is",5.744));
	       cameraModelList.add(new Pair("canoncanonpowershotsd200",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("canoncanonpowershotsd300",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("canoncanonpowershotsd400",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("canoncanonpowershotsd450",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("canoncanonpowershotsd500",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("canoncanonpowershotsd550",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("canoncanonpowershotsd600",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("canoncanonpowershotsd630",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("canoncanonpowershotsd700is",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("canoncanonpowershotsd750",5.75/*1/2.5*/));
	       cameraModelList.add(new Pair("canoncanonpowershotsd770is",6.16));
	       cameraModelList.add(new Pair("canoncanonpowershotsd780is",6.17));
	       cameraModelList.add(new Pair("canoncanonpowershotsd790is",6.16));
	       cameraModelList.add(new Pair("canoncanonpowershotsd800is",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("canoncanonpowershotsd850is",5.75));
	       cameraModelList.add(new Pair("canoncanonpowershotsd870is",5.75));
	       cameraModelList.add(new Pair("canoncanonpowershotsd890is",6.16/*1/2.3*/));
	       cameraModelList.add(new Pair("canoncanonpowershotsd950is",7.60));
	       cameraModelList.add(new Pair("canoncanonpowershotsd980is",6.16));
	       cameraModelList.add(new Pair("canoncanonpowershotsd990is",7.49));
	       cameraModelList.add(new Pair("canoncanonpowershotsx10is",6.16));
	       cameraModelList.add(new Pair("canoncanonpowershotsx100is",5.744));
	       cameraModelList.add(new Pair("canoncanonpowershotsx200is",6.16));
	       cameraModelList.add(new Pair("canoncanoscan8400f",0/*#scanner?*/));
	       cameraModelList.add(new Pair("canoneos300ddigital",22.66));
	       cameraModelList.add(new Pair("canoneosdigitalrebel",22.66));
	       cameraModelList.add(new Pair("canonpowershota510",5.76/*1/2.5"???*/));
	       cameraModelList.add(new Pair("canonpowershots30",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("casiocomputerco.,ltdex-s3",7.18));
	       cameraModelList.add(new Pair("casiocomputerco.,ltdex-z30",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("casiocomputerco.,ltdex-z750",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("casiocomputerco.,ltdqv-r40",7.18));
	       cameraModelList.add(new Pair("casiocomputerco.,ltd.ex-s500",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("casiocomputerco.,ltd.ex-s770",5.744));
	       cameraModelList.add(new Pair("casiocomputerco.,ltd.ex-z1000",7.716/*1/1.8*/));
	       cameraModelList.add(new Pair("casiocomputerco.,ltd.ex-z1050",7.36));
	       cameraModelList.add(new Pair("casiocomputerco.,ltd.ex-z110",5.75));
	       cameraModelList.add(new Pair("casiocomputerco.,ltd.ex-z60",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("casiocomputerco.,ltd.ex-z600",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("casiocomputerco.,ltd.ex-z700",5.744));
	       cameraModelList.add(new Pair("casiocomputerco.,ltd.ex-z850",7.176));
	       cameraModelList.add(new Pair("d-gateoutput:bmc-to-xml",0/*noidea*/));
	       cameraModelList.add(new Pair("eastmankodakcompanykodakcx6330zoomdigitalcamera",5.47));
	       cameraModelList.add(new Pair("eastmankodakcompanykodakcx7330zoomdigitalcamera",5.27/*1/2.7*/));
	       cameraModelList.add(new Pair("eastmankodakcompanykodakcx7525zoomdigitalcamera",0/*couldn'tfindonline*/));
	       cameraModelList.add(new Pair("eastmankodakcompanykodakcx7530zoomdigitalcamera",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("eastmankodakcompanykodakdx3900zoomdigitalcamera",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("eastmankodakcompanykodakdx4900zoomdigitalcamera",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("eastmankodakcompanykodakdx6340zoomdigitalcamera",5.27/*1/2.7*/));
	       cameraModelList.add(new Pair("eastmankodakcompanykodakdx6490zoomdigitalcamera",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("eastmankodakcompanykodakdx7630zoomdigitalcamera",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("eastmankodakcompanykodakeasysharecx4230zoomdigitalcam",5.50/*aguess:focal=5.5mm&widestangleis36mm*/));
	       cameraModelList.add(new Pair("eastmankodakcompanykodakeasysharem753zoomdigitalcamer",5.744));
	       cameraModelList.add(new Pair("eastmankodakcompanykodakeasysharez710zoomdigitalcamer",5.744));
	       cameraModelList.add(new Pair("eastmankodakcompanykodakeasysharezd710zoomdigitalcame",5.75));
	       cameraModelList.add(new Pair("eastmankodakcompanykodakz612zoomdigitalcamera",5.744));
	       cameraModelList.add(new Pair("eastmankodakcompanykodakz650zoomdigitalcamera",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("eastmankodakcompanykodakz700zoomdigitalcamera",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("eastmankodakcompanykodakz712iszoomdigitalcamera",6.20));
	       cameraModelList.add(new Pair("eastmankodakcompanykodakz740zoomdigitalcamera",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("fujifilmfinepixa310",5.27/*1/2.7*/));
	       cameraModelList.add(new Pair("fujifilmfinepixa330",5.27/*1/2.7*/));
	       cameraModelList.add(new Pair("fujifilmfinepixa360",5.744));
	       cameraModelList.add(new Pair("fujifilmfinepixa500",5.75));
	       cameraModelList.add(new Pair("fujifilmfinepixa600",7.600/*1/1.7*/));
	       cameraModelList.add(new Pair("fujifilmfinepixe500",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("fujifilmfinepixe510",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("fujifilmfinepixe550",7.600/*1/1.7*/));
	       cameraModelList.add(new Pair("fujifilmfinepixe900",7.78/*1/1.6*/));
	       cameraModelList.add(new Pair("fujifilmfinepixf10",7.600/*1/1.7*/));
	       cameraModelList.add(new Pair("fujifilmfinepixf20",7.60));
	       cameraModelList.add(new Pair("fujifilmfinepixf200exr",7.78));
	       cameraModelList.add(new Pair("fujifilmfinepixf30",7.600/*1/1.7*/));
	       cameraModelList.add(new Pair("fujifilmfinepixf450",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("fujifilmfinepixf47fd",7.78));
	       cameraModelList.add(new Pair("fujifilmfinepixf50fd",7.78));
	       cameraModelList.add(new Pair("fujifilmfinepixf601zoom",7.600/*1/1.7*/));
	       cameraModelList.add(new Pair("fujifilmfinepixf650",5.75));
	       cameraModelList.add(new Pair("fujifilmfinepixf70exr",6.40));
	       cameraModelList.add(new Pair("fujifilmfinepixs1000fd",6.16));
	       cameraModelList.add(new Pair("fujifilmfinepixs1500",6.16));
	       cameraModelList.add(new Pair("fujifilmfinepixs3pro",23.0));
	       cameraModelList.add(new Pair("fujifilmfinepixs5000",5.27/*1/2.7*/));
	       cameraModelList.add(new Pair("fujifilmfinepixs5200",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("fujifilmfinepixs5500",5.27/*1/2.7*/));
	       cameraModelList.add(new Pair("fujifilmfinepixs5600",5.75));
	       cameraModelList.add(new Pair("fujifilmfinepixs5700s700",5.75));
	       cameraModelList.add(new Pair("fujifilmfinepixs6500fd",7.600/*1/1.7*/));
	       cameraModelList.add(new Pair("fujifilmfinepixs7000",7.600/*1/1.7*/));
	       cameraModelList.add(new Pair("fujifilmfinepixs9000",8));
	       cameraModelList.add(new Pair("fujifilmfinepixz1",5.75));
	       cameraModelList.add(new Pair("fujifilmfinepixz2",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("fujifilmfinepix1400zoom",5.17));
	       cameraModelList.add(new Pair("fujifilmfinepix2600zoom",5.27/*1/2.7*/));
	       cameraModelList.add(new Pair("fujifilmfinepix2650",5.314));
	       cameraModelList.add(new Pair("fujifilmfinepix40i",7.600/*1/1.7*/));
	       cameraModelList.add(new Pair("fujifilmfinepix4800zoom",7.49));
	       cameraModelList.add(new Pair("fujifilmfinepixs2pro",23));
	       cameraModelList.add(new Pair("fullframefullframe",36/*ifweknowfocallengthinpixelsor35mmequivalent,write35mmequivalentintagsandwritemake=fullframe,model=fullframe*/));
	       cameraModelList.add(new Pair("hewlett-packardhpphotosmartr740",5.75));
	       cameraModelList.add(new Pair("hewlett-packardcompanyhpphotosmart315",5.3));
	       cameraModelList.add(new Pair("hewlett-packardhp635digitalcamera",4.54/*1/3.2*/));
	       cameraModelList.add(new Pair("hewlett-packardhpphotosmart43xseries",5.27/*1/2.7*/));
	       cameraModelList.add(new Pair("hewlett-packardhpphotosmart618(v1.1)",5.27/*1/2.7*/));
	       cameraModelList.add(new Pair("hewlett-packardhpphotosmart735",5.27));
	       cameraModelList.add(new Pair("hewlett-packardhpphotosmartc945(v01.54)",7.18));
	       cameraModelList.add(new Pair("hewlett-packardhpphotosmartc945(v01.61)",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("hewlett-packardhpphotosmartr507(v01.00)",5.744));
	       cameraModelList.add(new Pair("hewlett-packardhpphotosmartr707(v01.00)",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("hewlett-packardhpphotosmartr707(v01.00)+",7.144));
	       cameraModelList.add(new Pair("konicamiloltadynax5d",23.5));
	       cameraModelList.add(new Pair("konicaminoltadimagez5",5.75));
	       cameraModelList.add(new Pair("konicaminoltadynax5d",23.5));
	       cameraModelList.add(new Pair("konicaminoltadynax7d",23.5));
	       cameraModelList.add(new Pair("konicaminoltacamera,inc.dimagea2",8.80/*2/3*/));
	       cameraModelList.add(new Pair("konicaminoltacamera,inc.dimageg400",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("konicaminoltacamera,inc.dimagez2",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("konicaminoltadimagea200",8.80/*2/3*/));
	       cameraModelList.add(new Pair("konicaminoltadimagex1",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("konicaminoltadimagex50",5.744));
	       cameraModelList.add(new Pair("konicaminoltadimagez5",5.75));
	       cameraModelList.add(new Pair("konicaminoltaphotoimaging,idimageg600",7.18/*1/1.8*/));
	       cameraModelList.add(new Pair("minoltaco.,ltd.dimage7",8.80/*2/3*/));
	       cameraModelList.add(new Pair("minoltaco.,ltd.dimage7hi",8.80));
	       cameraModelList.add(new Pair("minoltaco.,ltd.dimage7i",8.80));
	       cameraModelList.add(new Pair("minoltaco.,ltd.dimagef100",7.176/*1/2.7*/));
	       cameraModelList.add(new Pair("minoltaco.,ltd.dimagexi",5.27/*1/2.7*/));
	       cameraModelList.add(new Pair("minoltaco.,ltd.dimagext",5.27/*1/2.7*/));
	       cameraModelList.add(new Pair("minoltaco.,ltd.dimagez1",5.27/*1/2.7*/));
	       cameraModelList.add(new Pair("minoltaco.,ltddimagex",5.27/*1/2.7*/));
	       cameraModelList.add(new Pair("nikoncoolpixl1",5.75));
	       cameraModelList.add(new Pair("nikoncoolpixl10",5.744));
	       cameraModelList.add(new Pair("nikoncoolpixl3",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("nikoncoolpixl4",5.75));
	       cameraModelList.add(new Pair("nikoncoolpixl5",5.744));
	       cameraModelList.add(new Pair("nikoncoolpixp1",7.18));
	       cameraModelList.add(new Pair("nikoncoolpixp2",7.176/*1/1.8*/));
	       cameraModelList.add(new Pair("nikoncoolpixp90",6.12));
	       cameraModelList.add(new Pair("nikoncoolpixs210",5.744));
	       cameraModelList.add(new Pair("nikoncoolpixs4",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("nikoncoolpixs4000",6.17));
	       cameraModelList.add(new Pair("nikoncoolpixs50",5.744));
	       cameraModelList.add(new Pair("nikoncoolpixs7c",5.76/*1/2.5*/));
	       cameraModelList.add(new Pair("nikoncorporationnikond1",23.7));
	       cameraModelList.add(new Pair("nikoncorporationnikond100",23.7));
	       cameraModelList.add(new Pair("nikoncorporationnikond1h",23.7));
	       cameraModelList.add(new Pair("nikoncorporationnikond200",23.6));
	   }
	
	   private void default_config() {
		   // Author:     Oliver Whyte <oliver.whyte@ens.fr>
		   // Date:       January 2012
		   // Copyright:  2012, Oliver Whyte
		   // Reference:  O. Whyte, J. Sivic, A. Zisserman, and J. Ponce. "Non-uniform Deblurring for Shaken Images". IJCV, 2011 (accepted).
		   //             O. Whyte, J. Sivic and A. Zisserman. "Deblurring Shaken and Partially Saturated Images". In Proc. CPCV Workshop at ICCV, 2011.
		   // URL:        http://www.di.ens.fr/willow/research/deblurring/, http://www.di.ens.fr/willow/research/saturation/

		   // Max dimension at finest scale
		   int s;
		   max_dim = 1024;
		   synthetic = false;

		   // Root directory to store results
		   //results_root = pwd;
		   if (results_dir == null) {
		       results_dir = System.getProperty("user.dir");
		   }

		   // Default config_name is image file name
		   config_name = blurredImage.getImageName();
		   
		   // New imresize function causes trouble, this should ultimately be changed
		   //if exist('imresize_old') == 2
		   //	imresizefn = @imresize_old;
		   //else
		   //	imresizefn = @imresize;
		   //end

		   // Visual feedback of algorithm running
		   do_display = false;

		   // Save out images of all the intermediate results?
		   //   0 = no, 1 = save deblurred result at each level, 2 = save all intermediate images
		   save_intermediate_images  = 0;
		   // Save pyramids and parameters for entire process into a .mat file?
		   save_mat = true;
		   // Save pyramids after each scale?
		   save_intermediate_pyramids = false;

		   // Is image raw (linear) or jpg (with gamma curve applied)
		   israw = false;

		   // Parameters of bilateral filter
		   bi_sigma_spatial0    = 2;
		   bi_sigma_spatial0    = bi_sigma_spatial0/Math.sqrt(2); // reducing bi_sigma_spatial by factor of sqrt(2), due to change in jcbfilter
		   bi_sigma_range0      = 0.5; // should be set relative to range of values in image (especially if using raw images whose range is 256 times larger)
		   bi_size              = 5;

		   // Parameters of shock filter
		   shock_dt0            = 1;
		   shock_iters          = 1;

		   // Decrease parameters over iterations as suggested by Cho & Lee
		   param_decrease = 0.9;

		   // Parameters of gradient histogramming
		   grad_dir_bins = 4;
		   grad_dir_quant = Math.PI/grad_dir_bins;
		   grad_thresh_decrease = 0.9; // decrease threshold on gradient magnitudes at each iteration (Cho & Lee: 0.9)
		   r = 2; // factor to multiply necessary number of retained gradients by (Cho & Lee: 2)
          
		   // Spatially variant blur
		   non_uniform = true;
		   
		   // Spatial binning when histogramming gradients
		   if (non_uniform) {
		       num_vert_regions = 3;
		       num_horz_regions = 3;
		   }
		   else {
		       num_vert_regions = 1;
		       num_horz_regions = 1;
		   }

		   // Weights for gradient data terms from Shan et al. 2008
		   omega0 = 1.0;   // weight for image pixel values. omega = 1 / 2^q, where q is order of operator
		   omega1 = 0.5; // weight for 1st order image derivatives
		   omega2 = 0.25; // weight for 2nd order image derivatives

		   // Regularization weights for non-blind image estimation step
		   alpha = 0.0005; // latent image regularization weight (Cho & Lee: 0.1)
		   kf_lambda = 8e3; // Krishnan & Fergus lambda parameter
		   kf_exponent = 0.5; // Krishnan & Fergus sparse gradient exponent

		   // Threshold kernel elements at 1/kernel_threshold of the max value (Cho & Lee: 20)
		   kernel_threshold = 20;

		   // Regularization weight for kernel estimation step
		   beta = 5; // kernel regularization weight (Cho & Lee: 5) // deblur_book uses beta = 0.01
		   num_cg_iters = 5; // number of conjugate gradient iterations in kernel estimation

		   // Parameters relating to camera rotations
		   theta_pre[0] = 0.0; // Kernel pre-rotation to apply
		   theta_pre[1] = 0.0;
		   theta_pre[2] = 0.0;
		   pixels_per_theta_step = 1; // Kernel sampling resolution
		   // parameters for dimensions of non-uniform kernel
		   blur_x_lims[0] = -(int)Math.floor(((BLUR_KERNEL_SIZE)-1)/2);
		   blur_x_lims[1] = (int)Math.floor(((BLUR_KERNEL_SIZE)-1)/2);
		   blur_y_lims[0] = -(int)Math.floor(((BLUR_KERNEL_SIZE)-1)/2);
		   blur_y_lims[1] = (int)Math.floor(((BLUR_KERNEL_SIZE)-1)/2);
		   if (non_uniform) {
			   blur_z_lims[0] = -(int)Math.floor(((BLUR_KERNEL_SIZE)-1)/2);
			   blur_z_lims[1] = (int)Math.floor(((BLUR_KERNEL_SIZE)-1)/2);
		   }
		   else {
		   	   blur_z_lims[0] = 0;
		   	   blur_z_lims[1] = 0;
		   }

		   // Multiscale parameters
		   scale_ratio_i = 1.0/Math.sqrt(2.0); // scale ratio for image
		   scale_ratio_k = 1.0/Math.sqrt(2.0); // scale ratio for kernel (must equal scale_ratio_i for uniform blur)
		   max_levels = 9;

		   // Number of iterations at each scale (Cho & Lee: 7)
		   num_iters = new int[max_levels];
		   for (s = 0; s < max_levels; s++) {
			   num_iters[s] = 5;
		   }

		   // Recenter kernel after each iteration
		   recenter_kernel = 1;

		   // Dilate current active region of kernel when solving at next scale
		   kernel_dilate_radius = 1;

		   // Saturation threshold
		   sat_thresh = 235.5/256;

		   // Default methods for kernel estimation and image estimation
		   kernel_method = "lars";
		   image_method = "conjgrad";

		   // Do kernel estimation? (if not kernel will need to be loaded)
		   do_estimate_kernel = true;
		   // Do image estimation?
		   do_deblur = new boolean[max_levels];
		   // estimate image at all levels
		   for (s = 0; s < max_levels; s++) {
			   do_deblur[s] = true;
		   }
		   // Perform color deblurring at each level?
		   do_color = new boolean[max_levels];
		   do_color[0] = true; // last level only
		   for (s = 1; s < max_levels; s++) {
			   do_color[s] = false;
		   }

		   // What to estimate the kernel from? 'blind', or 'true' if true sharp image available
		   estimate_kernel_from = "blind";

		   // Number of iterations in iterative non-blind deblurring algorithms
		   deconv_maxit = 20;

		   // Do the kernel thresholding step suggested by Cho & Lee?
		   threshold_kernel = true; // LEAVE THIS ON!!

		   // Number of threads to use for multi-threaded computation of BtB
		   //setenv('OMP_NUM_THREADS','2');

		   // Variables which allow overriding focal length from EXIF tags
		   //focal_length_in_35mm_true = [];
		   //focal_length_in_35mm_shake = [];

		   // Which levels of the pyramid to start and end on?
		   first_level = -1; // -1 means start at the highest level
		   final_level = 1;

		   // What method to use to compute BtB?
		   BtB_method = "exact"; // 'exact' or 'eff'

		   // Max number of non zeros in kernel for lars-lasso
		   // Can be important to avoid using too much memory
		   max_nonzeros_w = 200;

		   update_saturation_mask = false;

		   // Convenience flag to enable / disable fast approximation in both kernel and image estimation steps
		   fast_approx = true;

		   // Default filenames for storing results
		   //filename_kernel = @(scale,iter,note) sprintf('s%02d_it%04d_%s.png',scale,iter,note);
		   //filename_image  = @(im,scale,iter,note) sprintf('s%02d_it%04d_%s.jpg',scale,iter,note);


   
	   }
    	
    	public void runAlgorithm() {
    		// From blind_deblur_map.m
    		// Author:     Oliver Whyte <oliver.whyte@ens.fr>
    		// Date:       January 2012
    		// Copyright:  2012, Oliver Whyte
    		// Reference:  O. Whyte, J. Sivic, A. Zisserman, and J. Ponce. "Non-uniform Deblurring for Shaken Images". IJCV, 2011 (accepted).
    		//             O. Whyte, J. Sivic and A. Zisserman. "Deblurring Shaken and Partially Saturated Images". In Proc. CPCV Workshop at ICCV, 2011.
    		// URL:        http://www.di.ens.fr/willow/research/deblurring/, http://www.di.ens.fr/willow/research/saturation/
    		
    		// In absence of a log file, print to screen (file descriptor 1)
    		double im_true_bw[][] = null;
    		double im_true_rgb[][][] = null;
    		double im_blurry_bw[][] = null;
    		double im_blurry_rgb[][][] = null;
    		int size_true_row;
    		int size_true_col;
    		int sliceSize_true;
    		int size_blurry_row;
    		int size_blurry_col;
    		int sliceSize_blurry;
    		double buffer[];
    		int x,y;
    		default_config();
    		Logger logger = null;
    		if (logFile != null) {
    			logger = Logger.getLogger("DeblurLog");
    			FileHandler fh;
    			try {
    			    fh = new FileHandler(logFile);
    			    logger.addHandler(fh);
    			    SimpleFormatter formatter = new SimpleFormatter();
    			    fh.setFormatter(formatter);
    			    // Use logger.info("statement") to log messages
    			}
    			catch (SecurityException e) {
    			    e.printStackTrace();
    			    setCompleted(false);
    			    return;
    			}
    			catch (IOException e) {
    				e.printStackTrace();
    				setCompleted(false);
    				return;
    			}
    		}
    		// Start timer
    		long totaltime = System.currentTimeMillis();
    		// For uniform blur, scale ratios must be equal
    		if (!non_uniform) {
    			scale_ratio_k = scale_ratio_i;
    		}
    		// Shortcut flag to set BtB_method and image_method
    		if (fast_approx) {
    		    BtB_method = "eff";
    		    image_method = "direct";
    		}
    		else {
    		    BtB_method = "exact";
    		    image_method = "conjgrad";
    		}
    		// Load images and estimate calibration parameters
    		if (trueImage != null) {
    		    size_true_row = trueImage.getExtents()[0];
    		    size_true_col = trueImage.getExtents()[1];
    		    sliceSize_true = size_true_row * size_true_col;
    		    buffer = new double[sliceSize_true];
    		    if (!trueImage.isColorImage()) {
    		    	try {
    		    		trueImage.exportData(0, sliceSize_true, buffer);
    		    	}
    		    	catch (IOException e) {
        				e.printStackTrace();
        				setCompleted(false);
        				return;
        			}
    		    	im_true_bw = new double[size_true_col][size_true_row];
    		    	for (y = 0; y < size_true_col; y++) {
    		    		for (x = 0; x < size_true_row; x++) {
    		    			im_true_bw[y][x] = buffer[x + y * size_true_row];
    		    		}
    		    	}
    		    } // if (!trueImage.isColorImage())
    		    else { // trueImage.isColorImage()
    		    	im_true_rgb = new double[size_true_col][size_true_row][3];
    		        try {
    		        	trueImage.exportRGBData(1, 0, sliceSize_true, buffer);
    		        }
    		        catch (IOException e) {
        				e.printStackTrace();
        				setCompleted(false);
        				return;
        			}
                    for (y = 0; y < size_true_col; y++) {
    		    		for (x = 0; x < size_true_row; x++) {
    		    			im_true_rgb[y][x][0] = buffer[x + y * size_true_row];
    		    		}
    		    	}
                    try {
    		        	trueImage.exportRGBData(2, 0, sliceSize_true, buffer);
    		        }
    		        catch (IOException e) {
        				e.printStackTrace();
        				setCompleted(false);
        				return;
        			}
                    for (y = 0; y < size_true_col; y++) {
    		    		for (x = 0; x < size_true_row; x++) {
    		    			im_true_rgb[y][x][1] = buffer[x + y * size_true_row];
    		    		}
    		    	}
                    try {
    		        	trueImage.exportRGBData(3, 0, sliceSize_true, buffer);
    		        }
    		        catch (IOException e) {
        				e.printStackTrace();
        				setCompleted(false);
        				return;
        			}
                    for (y = 0; y < size_true_col; y++) {
    		    		for (x = 0; x < size_true_row; x++) {
    		    			im_true_rgb[y][x][2] = buffer[x + y * size_true_row];
    		    		}
    		    	}
    		    } // else trueImage.isColorImage();
    		    trueImageFileName = trueImage.getImageFileName();
    		    int index = trueImageFileName.lastIndexOf(".");
    		    trueImageExtension = trueImageFileName.substring(index+1);
    		    if ((trueImageExtension.equalsIgnoreCase("tif")) || (trueImageExtension.equalsIgnoreCase("tiff"))) {
    		    	israw_true = true;
    		    }
    		    else {
    		    	israw_true = false;
    		    }
    		    if ((trueImageExtension.equalsIgnoreCase("jpg")) || (trueImageExtension.equalsIgnoreCase("jpeg"))) {
    		        focalLengthTrue = trueImage.getFileInfo(0).getFocalLength();
    		        focalLength35True = trueImage.getFileInfo(0).getFocalLength35();
    		        cameraMakeTrue = trueImage.getFileInfo(0).getCameraMake();
    		        cameraModelTrue = trueImage.getFileInfo(0).getCameraModel();
    		        exposureTimeTrue = trueImage.getFileInfo(0).getExposureTime();
    		        FNumberTrue = trueImage.getFileInfo(0).getFNumber();
    		        ISOSpeedRatingTrue = trueImage.getFileInfo(0).getISOSpeedRating();
    		    }
    		    // Defaults
    		    double f = 0.0;
    		    double a = 1.0;
    		    if (!Double.isNaN(focalLength35True)) {
    		        f = focalLength35True/36.0*Math.max(trueImage.getExtents()[0], trueImage.getExtents()[1]);	
    		    }
    		    if ((f == 0.0) && (!Double.isNaN(focalLengthTrue)) && (cameraModelTrue != null)) {
    		        if (cameraModelList == null) {
    		        	initCameraModelList();
    		        }
    		    }
    		} // if (trueImage != null)
    		/*
    		if exist('file_true','var')
    		    [Ktrue,a_true,im_true,respfn_loaded,invrespfn_loaded,initial_scale] = load_image_file(file_true,max_dim,israw,focal_length_in_35mm_true);
    		    size_true = size(im_true);
    		end
    		if ~exist('synthetic','var') || ~synthetic
    		    synthetic = false;
    		    % If blurry image has not already been loaded, load it
    		    if ~exist('im_blurry','var')
    		        % Load image and get exif data from the image, or from file_jpg
    		        [Kblur,a_blur,im_blurry,respfn_loaded,invrespfn_loaded,initial_scale] = load_image_file(file_shake,max_dim,israw,focal_length_in_35mm_shake);
    		    end
    		    size_blurry = size(im_blurry);
    		else
    		    Kblur = Ktrue;
    		    a_blur = a_true;
    		    size_blurry = size_true;
    		    % Ensure linear response function
    		    respfn = @(x,a) x;
    		    invrespfn = @(x,a) x;
    		end
    		% Output location
    		if non_uniform
    		    config_name = [config_name '_nonuni'];
    		else
    		    config_name = [config_name '_uni'];
    		end
    		if synthetic
    		    config_name = [config_name '_size' num2str(SYNTHETIC_BLUR_SIZE) '_noise' num2str(SYNTHETIC_NOISE_STD)];
    		end
    		if do_estimate_kernel
    		    config_name = [config_name '_k-' kernel_method '_BtB-' BtB_method '_b-' num2str(beta)];
    		else
    		    config_name = [config_name '_k-loaded'];
    		end
    		% If additional final deconvolution methods have been requested, add them to the default method
    		if exist('image_method_final','var')
    		    if iscell(image_method_final)
    		        image_method_final = {image_method, image_method_final{:}};
    		    else
    		        image_method_final = {image_method, image_method_final};
    		    end
    		else
    		    image_method_final = image_method;
    		end
    		if ~exist('deconv_maxit_final','var')
    		    deconv_maxit_final = deconv_maxit;
    		end
    		config_name = [config_name '_i-' image_method '_a-' num2str(alpha)];
    		if ~exist('output_name','var')
    		    output_name = config_name;
    		end
    		if ~exist('output_directory','var')
    		    output_directory = fullfile(results_root,output_name);
    		end
    		if ~exist(output_directory,'dir'), mkdir(output_directory); end
    		% If we don't want to estimate the kernel, we must load a previously estimated one
    		if ~do_estimate_kernel
    		    try
    		        prev_load = load(file_load_kernel);
    		        pyr_kernel = prev_load.pyr_kernel;
    		        pyr_tgs    = prev_load.pyr_tgs;
    		        pyr_tt     = prev_load.pyr_tt;
    		        s_kernel_init = 1;
    		        while ~any(pyr_kernel{s_kernel_init}(:))
    		            s_kernel_init = s_kernel_init + 1;
    		        end
    		    catch
    		        error('Problem loading results file containing previously estimated kernel');
    		    end
    		    if prev_load.BLUR_KERNEL_SIZE ~= BLUR_KERNEL_SIZE
    		        error('Specified kernel size does not match size of loaded kernel');
    		    end
    		    if ~isequal(size(prev_load.pyr_blurry{1}),size_blurry(1:2))
    		        error('Image size does not match image size from loaded kernel');
    		    end
    		    clear prev_load;
    		end
    		
    		% Gamma correction function
    		if ~exist('respfn','var') && ~exist('invrespfn','var')
    		    % Use loaded functions -- ignore resp_params
    		    invrespfn = @(x,a) invrespfn_loaded(x);
    		    respfn    = @(x,a) respfn_loaded(x);
    		end
    		if ~exist('resp_params','var')
    		    resp_params = [];
    		end
    		% If using raw image, use sRGB curve for display
    		if strcmp(file_shake(end-3:end),'tiff') || strcmp(file_shake(end-3:end),'.tif')
    		    respfn_disp = @(x,a) linear2srgb(min(max(x,0),1));
    		else
    		    respfn_disp = @(x,a) respfn(min(max(x,0),1),a);
    		end
    		
    		% Save all the parameters into the results file
    		if save_mat
    		    save(fullfile(output_directory,'results.mat'));
    		end
    		
    		Ksharp = Kblur;
    		flb = Kblur(1,1);
    		x0b = Kblur(1,3);
    		y0b = Kblur(2,3);
    		max_radius = max(sqrt(([1, size_blurry(2), 1, size_blurry(2)]-x0b).^2 + ([1, 1, size_blurry(1), size_blurry(1)]-y0b).^2));
    		blur_x_lims = round(blur_x_lims * initial_scale);
    		blur_y_lims = round(blur_y_lims * initial_scale);
    		blur_z_lims = round(blur_z_lims * initial_scale);
    		BLUR_KERNEL_SIZE = ceil(BLUR_KERNEL_SIZE * initial_scale);
    		num_levels = max_levels;
    		% Make kernel pyramid ===========================================================
    		if non_uniform
    		    % Make angle and kernel pyramid
    		    dm = max(abs([1-x0b, size_blurry(2)-x0b, 1-y0b, size_blurry(1)-y0b]));
    		    % theta_grid_size = calculate_theta_grid_size([obs_imy,obs_imx],Kblur_all,pixels_per_theta_step);
    		    theta_grid_size = pixels_per_theta_step * flb / (flb^2 + dm^2);
    		    tgsx = theta_grid_size;
    		    tgsy = theta_grid_size;
    		    tgsz = pixels_per_theta_step / max_radius;
    		    if exist('tgs_z_factor','var'), tgsz = tgsz * tgs_z_factor; end
    		    theta_x_lims = blur_x_lims * flb / (flb^2 + dm^2);
    		    theta_y_lims = blur_y_lims * flb / (flb^2 + dm^2);
    		    theta_z_lims = blur_z_lims / max_radius;
    		else
    		    theta_grid_size = 1;
    		    tgsx = theta_grid_size;
    		    tgsy = theta_grid_size;
    		    tgsz = pixels_per_theta_step / max_radius;
    		    theta_x_lims = blur_x_lims;
    		    theta_y_lims = blur_y_lims;
    		    theta_z_lims = blur_z_lims;
    		end
    		if do_estimate_kernel
    		    [pyr_kernel,pyr_tt,pyr_tgs] = make_kernel_pyramid(theta_x_lims,theta_y_lims,theta_z_lims,[tgsy,tgsx,tgsz],scale_ratio_k,num_levels,non_uniform);
    		elseif ~exist('pyr_kernel','var')
    		    [pyr_kernel,pyr_tt,pyr_tgs] = make_kernel_pyramid(theta_x_lims,theta_y_lims,theta_z_lims,[tgsy,tgsx,tgsz],scale_ratio_k,num_levels,non_uniform);
    		else
    		    [pyr_kernel,pyr_tt,pyr_tgs] = make_kernel_pyramid(theta_x_lims,theta_y_lims,theta_z_lims,[tgsy,tgsx,tgsz],scale_ratio_k,num_levels,non_uniform,pyr_kernel{s_kernel_init},1);
    		    % Threshold kernel after downsampling
    		    if threshold_kernel
    		        for s=1:length(pyr_kernel)
    		            pyr_kernel{s}(pyr_kernel{s} < max(pyr_kernel{s}(:))/kernel_threshold) = 0;
    		            pyr_kernel{s} = pyr_kernel{s} / sum(pyr_kernel{s}(:));
    		        end
    		    end
    		end
    		num_levels = length(pyr_kernel);
    		
    		% Generate synthetically blurred image ==========================================
    		if synthetic
    		    if strcmp(synthetic_type,'z')
    		        % synth_angle = blur_z_lims(2)-blur_z_lims(1))/max_radius*0.75*180/pi;
    		        synth_angle = 2*floor((SYNTHETIC_BLUR_SIZE-1)/2)/max_radius*0.75*180/pi;
    		    elseif strcmp(synthetic_type,'y') || strcmp(synthetic_type,'x')
    		        synth_angle = 2*floor((SYNTHETIC_BLUR_SIZE-1)/2)/flb*0.75*180/pi;
    		    end
    		    init_tt = shake_camera(zeros(size_blurry(1:2)),Kblur,size_blurry(1:2),Kblur,synthetic_type,synth_angle);
    		    im_blurry = apply_blur_kernel_mex(im_true,size(im_true),Ksharp,Kblur,-init_tt,ones(size(init_tt,2),1)/size(init_tt,2),0,non_uniform,-theta_pre);
    		    im_blurry = max(min(1,im_blurry + SYNTHETIC_NOISE_STD/256*randn(size(im_blurry))),0);
    		    if save_mat
    		        save(fullfile(output_directory,'synthetic_blur.mat'),'im_blurry');
    		    end
    		    [pyr_kernel_true] = make_kernel_pyramid(theta_x_lims,theta_y_lims,theta_z_lims,[tgsy,tgsx,tgsz],scale_ratio_k,num_levels,non_uniform,[],1,init_tt);
    		    if non_uniform
    		        w_true = pyr_kernel_true{1};
    		    else
    		        w_true = sum(pyr_kernel_true{1},3);
    		        [pyr_kernel_true] = make_kernel_pyramid(theta_x_lims,theta_y_lims,theta_z_lims,[tgsy,tgsx,tgsz],scale_ratio_k,num_levels,non_uniform,w_true,1);
    		    end
    		end
    		
    		% Conversion to grayscale
    		rgb2gray_local = @(im) mean(im,3);
    		
    		blurry_unsaturated_colour = im_blurry < sat_thresh;
    		blurry_unsaturated = max(im_blurry,[],3) < sat_thresh;
    		
    		im_blurry_colour = im_blurry;
    		im_blurry = rgb2gray_local(im_blurry);
    		if exist('im_true','var')
    		    im_true_colour = im_true;
    		    im_true = rgb2gray_local(im_true);
    		end
    		
    		% Make image pyramid ============================================================
    		% PYRAMIDS ALWAYS HOLD JPG VALUES
    		pyr_blurry = cell(1);
    		pyr_blurry{1} = im_blurry;
    		pyr_blurry_colour = cell(1);
    		pyr_blurry_colour{1} = im_blurry_colour;
    		pyr_saturation_mask{1} = blurry_unsaturated;
    		pyr_saturation_mask_colour{1} = blurry_unsaturated_colour;
    		pyr_Kblur = cell(1);
    		pyr_Kblur{1} = Kblur;
    		if exist('im_true','var')
    		    pyr_true = cell(1);
    		    pyr_true{1} = im_true;
    		    pyr_true_colour = cell(1);
    		    pyr_true_colour{1} = im_true_colour;
    		end
    		for s=2:num_levels
    		    scale_factor = scale_ratio_i^(s-1);
    		    ib_s = imresizefn(pyr_blurry{1},scale_factor,'bilinear');
    		    [hhs,wws] = size(ib_s(:,:,1));
    		    if max(size(ib_s)) < 25; break; end
    		    pyr_blurry{s} = ib_s;
    		    pyr_blurry_colour{s} = imresizefn(pyr_blurry_colour{1},scale_factor,'bilinear');
    		    if exist('im_true','var')
    		        pyr_true{s} = imresizefn(pyr_true{1},scale_factor,'bilinear');
    		        pyr_true_colour{s} = imresizefn(pyr_true_colour{1},scale_factor,'bilinear');
    		    end
    		    % Use new imresize, as old one is very strange when it comes to resizing binary functions
    		    if ~exist('imresize_old'), warning('Strange behaviour may occur from resizing saturation masks, due to use of "old" imresize function'); end
    		    pyr_saturation_mask{s} = imresize(uint8(blurry_unsaturated*256),[hhs,wws],'bilinear') == 255;
    		    pyr_saturation_mask_colour{s} = imresize(uint8(blurry_unsaturated_colour*256),[hhs,wws],'bilinear') == 255;
    		    % Make calibration for each scale
    		    pyr_Kblur{s} = htranslate([0.5,0.5])*hscale([scale_factor,scale_factor])*htranslate([-0.5,-0.5])*Kblur;
    		end
    		num_levels = length(pyr_blurry);
    		% By default, start at the coarsest level of the pyramid
    		if first_level == -1
    		    first_level = num_levels;
    		else
    		    first_level = min(first_level,num_levels);
    		end
    		for s=1:num_levels
    		    % Initialise to blurry image
    		    pyr_deblurred{s} = pyr_blurry{s};
    		    pyr_deblurred_colour{s} = pyr_blurry_colour{s};
    		end
    		% Make mask of reliably observed pixels
    		pyr_mask = cell(1);
    		
    		% Save all input pyramids
    		if save_mat
    		    save([output_directory '/results.mat'],'pyr_blurry','pyr_deblurred','pyr_blurry_colour','pyr_deblurred_colour','pyr_kernel','pyr_tt','pyr_saturation_mask','pyr_saturation_mask_colour','pyr_mask','pyr_tgs','pyr_Kblur','-append');
    		    if exist('im_true','var')
    		        save([output_directory '/results.mat'],'pyr_true','pyr_true_colour','-append');
    		    end
    		    if synthetic
    		        save([output_directory '/results.mat'],'pyr_kernel_true','-append');
    		    end
    		end
    		
    		% Derivatives filters (for convolution), always useful
    		keye = 1;
    		kx = [0, 1,-1, 0, 0];  ky = kx';
    		kxx = conv2(kx,kx,'same'); kyy = kxx'; kxy = conv2(kx,ky,'full');
    		kxt = rot90(rot90(kx));   kyt = kxt';
    		kxxt = rot90(rot90(kxx)); kxyt = rot90(rot90(kxy)); kyyt = kxxt';
    		
    		% Set bilateral range sigma relative to range of image, extracted from middle 80% of pixels
    		tmp = sort(pyr_blurry{1}(:));
    		range_blurry = (tmp(ceil(0.9*length(tmp))) - tmp(1+floor(0.1*length(tmp)))) / 0.8;
    		bi_sigma_range0 = bi_sigma_range0 * range_blurry;
    		
    		% If do_deblur specified as a single boolean value, replicate it to all scales
    		if length(do_deblur) == 1, do_deblur = repmat(do_deblur,[1 num_levels]); end
    		% If num_iters specified as a single value, replicate it to all scales
    		if length(num_iters) == 1, num_iters = repmat(num_iters,[1 num_levels]); end
    		% Cumulative recentering of the true image
    		H_recenter_cumul = eye(3);
    		R_recenter_cumul = eye(3);
    		
    		if do_display
    		    figure(1); clf;
    		end
    		for s = first_level:-1:final_level
    		    fprintf(logfile,'Scale %d of %d\n',s,num_levels);
    		    scale_factor_i = scale_ratio_i^(s-1);
    		    % Reset parameters to their initial values
    		    bi_sigma_range   = bi_sigma_range0;
    		    bi_sigma_spatial = bi_sigma_spatial0;
    		    shock_dt         = shock_dt0;
    		    BC = invrespfn(pyr_blurry_colour{s},resp_params);
    		    % Gray blurry image at this scale
    		    B = rgb2gray_local(BC);
    		    BCx  = imfilter(BC,kx ,'conv','replicate');   BCy  = imfilter(BC,ky ,'conv','replicate');
    		    BCxx = imfilter(BC,kxx,'conv','replicate');   BCxy = imfilter(BC,kxy,'conv','replicate');
    		    BCyy = imfilter(BC,kyy,'conv','replicate');
    		    if do_deblur(s) && (s == final_level || save_intermediate_images>0)
    		        if do_colour(s)
    		            imwrite(respfn_disp(BC,resp_params),fullfile(output_directory,filename_image(BC,s,0,'blurry')),'jpg','Quality',100);
    		        else
    		            imwrite(respfn_disp(B,resp_params),fullfile(output_directory,filename_image(B,s,0,'blurry')),'jpg','Quality',100);
    		        end
    		    end
    		    Bx  = imfilter(B,kx ,'conv','replicate');   By  = imfilter(B,ky ,'conv','replicate');
    		    Bxx = imfilter(B,kxx,'conv','replicate');   Bxy = imfilter(B,kxy,'conv','replicate');
    		    Byy = imfilter(B,kyy,'conv','replicate');
    		    [hB ,wB ] = size(B);            nB = hB*wB;     [xxB,yyB] = meshgrid(1:wB,1:hB);
    		    % True image at this scale
    		    if exist('im_true','var')
    		        T0        = invrespfn(pyr_true{s},resp_params);
    		        if recenter_kernel
    		            T = apply_homography_image_mex(T0,H_recenter_cumul,1:size(T0,1),1:size(T0,2),0);
    		        else
    		            T = T0;
    		        end
    		        if s == final_level || save_intermediate_images>0
    		            if do_colour(s)
    		                imwrite(pyr_true_colour{s},fullfile(output_directory,filename_image(pyr_true_colour{s},s,0,'true')),'jpg','Quality',100);
    		            else
    		                imwrite(respfn_disp(T,resp_params),fullfile(output_directory,filename_image(T,s,0,'true')),'jpg','Quality',100);
    		            end
    		        end
    		        Tx  = imfilter(T,kx ,'conv','replicate');   Ty  = imfilter(T,ky ,'conv','replicate');
    		        Txx = imfilter(T,kxx,'conv','replicate');   Txy = imfilter(T,kxy,'conv','replicate');
    		        Tyy = imfilter(T,kyy,'conv','replicate');
    		        [hT ,wT ] = size(T);            nT = hT*wT;     [xxT,yyT] = meshgrid(1:wT,1:hT);
    		    end
    		    % Estimated latent image at this scale
    		    L_new  = invrespfn(pyr_deblurred{s},resp_params);
    		    LC_new = invrespfn(pyr_deblurred_colour{s},resp_params);
    		    if do_deblur(s) && save_intermediate_images==2
    		        imwrite(respfn_disp(L_new,resp_params),fullfile(output_directory,filename_image(L_new,s,0,'upsampled')),'jpg','Quality',100);
    		    end
    		    [hL,wL]   = size(L_new);        nL = hL*wL;     [xxL,yyL] = meshgrid(1:wL,1:hL);
    		    % Estimated kernel at this scale
    		    if s==first_level && do_estimate_kernel
    		        w_new = zeros(size(pyr_kernel{s}));
    		        w_new(ceil(numel(w_new)/2)) = 1;
    		        w_new = w_new(:);
    		    else
    		        w_new = pyr_kernel{s}(:);
    		    end
    		    if synthetic
    		        w_true_s = pyr_kernel_true{s};
    		    end
    		    BLUR_KERNEL_SIZE_s = ceil((BLUR_KERNEL_SIZE-1)/2*scale_factor_i)*2 + 1;
    		    if save_intermediate_images==2
    		        if non_uniform
    		            if usejava('jvm') && ~isempty(getenv('DISPLAY')) % plotting require a display, and Java Virtual Machine to call `figure;`
    		                plot_nonuni_kernel_write(w_new,pyr_tt(s,:),fullfile(output_directory,filename_kernel(s,0,'kernel')));
    		            end
    		            imwrite(plotFiltersEFF(makeEFF(size(pyr_deblurred{s}),thetagrid2list(pyr_tt(s,:),w_new(:)~=0),[6 8],pyr_Kblur{s}),w_new(w_new(:)~=0)),fullfile(output_directory,filename_kernel(s,0,'filters')));
    		        else
    		            imwrite(reshape(w_new,size(pyr_tt{s,1}))/max(w_new(:)),fullfile(output_directory,filename_kernel(s,0,'kernel')));
    		        end
    		    end
    		    % Orientations for this scale
    		    nk_s  = numel(pyr_kernel{s});
    		    % Mask active region of kernel
    		    if s == first_level
    		        if kernel_dilate_radius > 0
    		            if kernel_dilate_radius < inf
    		                dilate_kernel = ones([repmat(1+2*kernel_dilate_radius,[1, ndims(pyr_tt{s,1})]), 1]);
    		                use_rotations = reshape(w_new > 0, size(pyr_tt{s,1}));
    		                use_rotations = convn(double(use_rotations),dilate_kernel,'same') >= 0.9;
    		            else
    		                use_rotations = true(numel(w_new),1);
    		            end
    		        end
    		    else
    		        use_rotations = w_new > 0;
    		    end
    		    if isinf(kernel_dilate_radius)
    		        use_rotations = true(numel(w_new),1);
    		    end
    		    use_rotations = logical(use_rotations(:));
    		    % Number of gradients to retain for kernel estimation... we have no exact 
    		    % equivalent of Cho & Lee's variable m
    		    % m     = nk_s; 
    		    m = BLUR_KERNEL_SIZE_s^2;
    		    % m = nnz(use_rotations); 
    		    % Orientations & calibration
    		    theta_s = [pyr_tt{s,2}(:), pyr_tt{s,1}(:), pyr_tt{s,3}(:)]';
    		    Kblur_s = pyr_Kblur{s};
    		    Ksharp_s = pyr_Kblur{s};
    		    % Mask out edges of the image
    		    pyr_mask{s} = conv2(ones(size(B)),ones(BLUR_KERNEL_SIZE_s),'same') == BLUR_KERNEL_SIZE_s^2;
    		    M = imerode(pyr_mask{s},ones(3));
    		    M = M & imerode(pyr_saturation_mask{s},ones(3),'same');
    		    MC = repmat(M,[1 1 3]);
    		    for c=1:3, MC(:,:,c) = MC(:,:,c) & imerode(pyr_saturation_mask_colour{s}(:,:,c),ones(3),'same'); end
    		    % Display upsampled kernel
    		    if do_display
    		        subplot(234); cla;
    		        if non_uniform
    		            % Can display the kernel or some equivalent filters, if you change this switch
    		            if usejava('jvm') && ~isempty(getenv('DISPLAY')) % plotting require a display, and Java Virtual Machine to call `figure;`
    		                plot_nonuni_kernel(w_new,pyr_tt(s,:),1,0,0,[],1);
    		            else
    		                w_vis = plotFiltersEFF(makeEFF(size(pyr_deblurred{s}),theta_s(:,w_new(:)~=0),[6 8],Kblur_s),w_new(w_new(:)~=0));
    		                imagesc(w_vis); 
    		                axis off; axis image;
    		                camproj('orthographic'); view(0,90);
    		                title('Kernel');
    		            end
    		        else 
    		            if synthetic
    		                w_vis = cat(3,zeros(size(w_true_s)),w_true_s/max(max(w_true_s(:)),1e-10),reshape(w_new/max(max(w_new(:)),1e-10),size(pyr_tt{s,1})));
    		            else
    		                w_vis = repmat(reshape(w_new/max(max(w_new(:)),1e-10),size(pyr_tt{s,1})),[1 1 3]);
    		            end
    		            imagesc(w_vis); 
    		            axis off; axis image;
    		            camproj('orthographic'); view(0,90);
    		            title('Kernel');
    		        end
    		        drawnow
    		    end
    		    % Main loop =================================================================
    		    for iter = 1:num_iters(s)
    		        fprintf(logfile,'\tIteration %d of %d\n',iter,num_iters(s));
    		        L_old = L_new;
    		        % Only do colour deblurring in the last iteration at a given scale
    		        do_colour_this_iter = do_colour(s) && iter==num_iters(s);
    		        if do_colour_this_iter
    		            LC_old = LC_new;
    		        end
    		        % Store old kernel
    		        w_old = w_new;
    		        final_iter_this_level = iter == num_iters(s);
    		        % Get image estimation method to use on this iteration
    		        if s == final_level && final_iter_this_level
    		            final_iter = true;
    		            image_method_this_iter = image_method_final;
    		            deconv_maxit_this_iter = deconv_maxit_final;
    		        else
    		            final_iter = false;
    		            image_method_this_iter = image_method;
    		            deconv_maxit_this_iter = deconv_maxit;
    		        end
    		        % Prediction ============================================================
    		        if do_estimate_kernel
    		            fprintf(logfile,'\tEdge prediction... ');
    		            switch estimate_kernel_from
    		            case 'blind'
    		                L_from = L_old;
    		            case 'true'
    		                L_from = T;
    		                % Lp = T;
    		            end
    		            % Bilateral filtering
    		            Lb = jcbfilter(L_from,L_from,bi_sigma_spatial,bi_sigma_range,bi_size);
    		            % Shock filtering
    		            Lp = shock_filter(Lb,shock_iters,shock_dt);
    		            if save_intermediate_images==2
    		                imwrite(respfn_disp(Lb,resp_params),fullfile(output_directory,filename_image(Lb,s,iter,'bilateral_filtered')),'jpg','Quality',100);
    		                imwrite(respfn_disp(Lp,resp_params),fullfile(output_directory,filename_image(Lp,s,iter,'shock_filtered')),'jpg','Quality',100);
    		            end
    		            if do_display
    		                figure(1);
    		                subplot(231); imagesc(respfn_disp(Lb,resp_params)); colormap gray; caxis([0 1]); axis image; axis off; title('Bilateral')
    		                subplot(232); imagesc(respfn_disp(Lp,resp_params)); colormap gray; caxis([0 1]); axis image; axis off; title('Shock')
    		            end
    		            % Gradient thresholding
    		            Lpx  = imfilter(Lp,kx ,'conv','replicate');   Lpy  = imfilter(Lp,ky ,'conv','replicate');
    		            Lpmag = sqrt(Lpx.^2 + Lpy.^2);  % Magnitudes
    		            Lpmag(~imerode(M,ones(BLUR_KERNEL_SIZE_s))) = 0;
    		            % Compute gradient threshold to use
    		            if iter==1 % then calculate threshold
    		                Lparg = atan2(Lpy,Lpx);         % Directions
    		                Lpbin = mod(Lparg,pi);          % Combine opposite directions
    		                Lpbin = ceil(Lpbin/grad_dir_quant); % Quantize directions
    		                % Find threshold that keeps r*m pixels from each bin
    		                [Lpmagsorted,ixsort] = sort(Lpmag(:),'descend');
    		                Lpbinsorted = Lpbin(ixsort);
    		                ixthresh = ones(num_vert_regions,num_horz_regions); % index of threshold in Lmagsorted
    		                grad_thresh = zeros(num_vert_regions,num_horz_regions);
    		                if do_display, region_borders = zeros(size(Lpmag)); end
    		                for xbin=1:num_horz_regions
    		                    for ybin=1:num_vert_regions
    		                        smask = false(size(Lp));
    		                        region_rows = round((ybin-1)*hL/num_vert_regions+1):round((ybin)*hL/num_vert_regions);
    		                        region_cols = round((xbin-1)*wL/num_horz_regions+1):round((xbin)*wL/num_horz_regions);
    		                        smask(region_rows,region_cols) = true;
    		                        smasksorted = smask(ixsort);
    		                        for bin=1:grad_dir_bins
    		                            % we need to include at least r*m pixels from each bin
    		                            % find index of r*m'th smallest gradient in this bin
    		                            if ~any(Lpbinsorted(:)==bin & smask(:)), error(sprintf('no entries in bin %d',bin)); end
    		                            ixbin = max(find(Lpbinsorted(:)==bin & smasksorted(:),ceil(r*m/num_horz_regions/num_vert_regions),'first'));
    		                            % make sure our threshold lies beyond that
    		                            ixthresh(ybin,xbin) = max(ixthresh(ybin,xbin),ixbin);
    		                        end
    		                        grad_thresh(ybin,xbin) = Lpmagsorted(ixthresh(ybin,xbin));
    		                        if do_display
    		                            if ybin==1, region_borders(region_rows(1),region_cols)=1; end
    		                            if xbin==1, region_borders(region_rows,region_cols(1))=1; end
    		                            region_borders(region_rows(end),region_cols)=1; region_borders(region_rows,region_cols(end))=1;
    		                        end
    		                    end
    		                end
    		            else
    		                grad_thresh = grad_thresh_decrease*grad_thresh;
    		            end
    		            % Perform gradient thresholding
    		            Px = Lpx;       Py = Lpy;
    		            for xbin=1:num_horz_regions
    		                for ybin=1:num_vert_regions
    		                    smask = false(size(Lp));
    		                    smask(round((ybin-1)*hL/num_vert_regions+1):round((ybin)*hL/num_vert_regions),...
    		                          round((xbin-1)*wL/num_horz_regions+1):round((xbin)*wL/num_horz_regions)) = true;
    		                    Px(smask & Lpmag < grad_thresh(ybin,xbin)) = 0; % threshold gradients
    		                    Py(smask & Lpmag < grad_thresh(ybin,xbin)) = 0; % threshold gradients
    		                end
    		            end
    		            % Delete isolated non-zeros
    		            Pnz = sum(Px~=0,3)~=0 | sum(Py~=0,3)~=0;
    		            Pnz = Pnz & conv2(double(Pnz),padImage(zeros(3),1*[1 1 1 1],1),'same');
    		            Px(~Pnz) = 0;   Py(~Pnz) = 0;
    		            fprintf(logfile,'done\n');
    		            % Optionally reconstruct the thresholded gradients into an image
    		            if do_display || save_intermediate_images==2
    		                Precon = poisson_blend_fft(Px,Py); Precon=(Precon-min(Precon(:)))/(max(Precon(:))-min(Precon(:)));
    		            end
    		            if do_display
    		                subplot(233); imagesc(Precon); colormap gray; axis image; axis off; title('Thresholded')
    		                drawnow
    		            end
    		        end
    		        % Decrease parameters
    		        bi_sigma_range   = param_decrease*bi_sigma_range;
    		        bi_sigma_spatial = bi_sigma_spatial/param_decrease;
    		        shock_dt         = param_decrease*shock_dt;
    		        % Estimate kernel =======================================================
    		        if do_estimate_kernel
    		            fprintf(logfile,'\tKernel estimation (%d / %d elements to be estimated)... ',nnz(use_rotations),numel(w_new));
    		            wtime=tic;
    		            Pxx = imfilter(Px,kx,'conv','replicate');   Pxy = imfilter(Px,ky,'conv','replicate');
    		            Pyy = imfilter(Py,ky,'conv','replicate');   Pyx = imfilter(Py,kx,'conv','replicate');
    		            % Run conjugate gradients to minimise ||A*w - b||^2 + beta*||w||^2
    		            Pall = cat(3,sqrt(omega1)*Px,sqrt(omega1)*Py,sqrt(omega2)*Pxx,sqrt(omega2)*Pyy,sqrt(omega2)*(Pxy+Pyx)/2);
    		            Ball = cat(3,sqrt(omega1)*Bx,sqrt(omega1)*By,sqrt(omega2)*Bxx,sqrt(omega2)*Byy,sqrt(omega2)*Bxy);
    		            if exist('im_true','var')
    		                Tall = cat(3,sqrt(omega1)*Tx,sqrt(omega1)*Ty,sqrt(omega2)*Txx,sqrt(omega2)*Tyy,sqrt(omega2)*Txy);
    		            end
    		            if non_uniform
    		                % Always use EFF to do this step, otherwise it wastes a lot of time
    		                Pnz_mask = blurEFF(double(sum(Pall~=0,3)>0),ones(nnz(use_rotations),1),theta_s(:,use_rotations),Kblur_s) > 0.5;
    		                % Pnz_mask = apply_blur_kernel_mex(double(sum(Pall~=0,3)>0),size(B),Ksharp_s,Kblur_s,-theta_s(:,use_rotations),ones(nnz(use_rotations),1),0,non_uniform,-theta_pre) > 0;
    		            else
    		                Pnz_mask = conv2(double(sum(Pall~=0,3)>0),double(reshape(use_rotations,size(w_new))),'same') > 0;
    		            end
    		            kernel_obs_mask = M & Pnz_mask;
    		            if do_display
    		                subplot(233); imagesc(cat(3,Precon.*kernel_obs_mask+~kernel_obs_mask,Precon.*~region_borders+region_borders,Precon)); colormap gray; axis image; axis off; title('Thresholded')
    		                drawnow
    		            end
    		            if save_intermediate_images==2
    		                imwrite(Precon,fullfile(output_directory,filename_image(Precon,s,iter,'thresholded_edges')),'jpg','Quality',100);
    		            end
    		            % Precompute B' * B and B' * g rather than computing the matrix-vector products on the fly
    		            if non_uniform
    		                switch BtB_method
    		                case 'eff'
    		                    [BtB,Btg] = BtB_EFF(Pall,Ball,theta_s(:,use_rotations),Kblur_s,M);
    		                case 'exact'
    		                    [BtB,Btg] = BtB_mex(Pall,Ball,kernel_obs_mask,Ksharp_s,Kblur_s,-theta_s(:,use_rotations),non_uniform);
    		                otherwise
    		                    error('Unknown method of pre-computing BtB')
    		                end
    		            else
    		                BtB = BtB_uni(Pall,Ball,w_new);
    		            end
    		            % Do kernel estimation
    		            switch kernel_method
    		            case 'conjgrad'
    		                % Add regularization
    		                BtB = sparse(BtB) + beta*speye(size(BtB));
    		                fprintf(logfile,'conjugate gradients... ');
    		                [w_new(use_rotations),pcg_flag] = pcg(BtB,Btg);
    		            case {'lars','lars_ols'}
    		                fprintf(logfile,'lars-lasso... ');
    		                [w_lars,w_reg_path] = lars(BtB, Btg, 1, beta/2, 1, 1, 0, 2, max_nonzeros_w);
    		                if strfind(kernel_method,'ols')
    		                    fprintf(logfile,'final least squares on active variables... ')
    		                    w_lars(w_lars~=0) = BtB(w_lars~=0,w_lars~=0) \ Btg(w_lars~=0);
    		                end
    		                w_new(use_rotations) = w_lars;
    		            otherwise
    		                error('Unknown kernel optimization method')
    		            end
    		            % Threshold kernel values
    		            if threshold_kernel
    		                w_new(w_new < max(w_new(:))/kernel_threshold) = 0;
    		            end
    		            % Check that kernel is not all zero
    		            if sum(w_new) == 0, w_new((numel(w_new)+1)/2)=1; warning('kernel has all zero elements -- resetting to delta'); end
    		            % Recenter kernel ===================================================
    		            if recenter_kernel && s>1
    		                % Centroid code below assumes that kernel sums to one
    		                w_new = w_new/sum(w_new(:));
    		                [tsy_s,tsx_s,tsz_s] = size(pyr_tt{s});
    		                w_center = reshape(w_new,[tsy_s,tsx_s,tsz_s]);
    		                % get centre of mass
    		                mu_y = sum((1:tsy_s)' .* reshape(sum(sum(w_center,2),3),[],1));
    		                mu_x = sum((1:tsx_s)' .* reshape(sum(sum(w_center,1),3),[],1));
    		                mu_z = sum((1:tsz_s)' .* reshape(sum(sum(w_center,1),2),[],1));
    		                % get mean offset
    		                offset_x = round( floor(tsx_s/2)+1 - mu_x );
    		                offset_y = round( floor(tsy_s/2)+1 - mu_y );
    		                offset_z = round( floor(tsz_s/2)+1 - mu_z );
    		                % make a 3d kernel to do translation
    		                shift_kernel = zeros(2*abs(offset_y)+1,2*abs(offset_x)+1,2*abs(offset_z)+1);
    		                shift_kernel(abs(offset_y)+1+offset_y,abs(offset_x)+1+offset_x,abs(offset_z)+1+offset_z) = 1;
    		                shift_indx = sub2ind([tsy_s,tsx_s,tsz_s],(tsy_s-1)/2+1+offset_y,(tsx_s-1)/2+1+offset_x,(tsz_s-1)/2+1+offset_z);
    		                shift_theta = [pyr_tt{s,2}(shift_indx);pyr_tt{s,1}(shift_indx);pyr_tt{s,3}(shift_indx)];
    		                % shift both image and blur kernel
    		                w_center = convn(w_center,shift_kernel,'same');
    		                use_rotations = logical(convn(double(use_rotations),shift_kernel,'same'));
    		                if non_uniform
    		                    R_recenter = expm(crossmatrix(shift_theta));
    		                    H_recenter = Kblur_s*R_recenter*inv(Kblur_s);
    		                else
    		                    H_recenter = htranslate(shift_theta);
    		                end
    		                % Recenter true sharp image, if we have one
    		                if strcmp(estimate_kernel_from,'true')
    		                    if non_uniform
    		                        R_recenter_cumul = R_recenter*R_recenter_cumul;
    		                        H_recenter_cumul = Kblur_s*R_recenter_cumul*inv(Kblur_s);
    		                    else
    		                        % For uniform, we must put the translations in terms of pixels at the highest scale
    		                        H_recenter_cumul = htranslate(shift_theta/scale_factor_i)*H_recenter_cumul;
    		                    end
    		                    T = apply_homography_image_mex(T0,H_recenter_cumul,1:hT,1:wT,0);
    		                end
    		                w_new = w_center(:);
    		            end
    		            % Dilate kernel =====================================================
    		            if kernel_dilate_radius > 0
    		                if kernel_dilate_radius < inf
    		                    dilate_kernel = ones([repmat(1+2*kernel_dilate_radius,[1, ndims(pyr_tt{s,1})]), 1]); % needs trailing 1 in dimensions for ones, otherwise a 1D kernel will call ones(n), creating an n x n matrix instead of an n x 1 matrix.
    		                    use_rotations = reshape(w_new > 0, size(pyr_tt{s,1}));
    		                    use_rotations = convn(double(use_rotations),dilate_kernel,'same') >= 0.9;
    		                else
    		                    use_rotations = true(size(w_new));
    		                end
    		            end
    		            use_rotations = logical(use_rotations(:));
    		            w_new = w_new/sum(w_new(:));
    		            % Display ===========================================================
    		            if do_display
    		                subplot(234); 
    		                if non_uniform
    		                    % Plot filters or plot kernel?
    		                    if usejava('jvm') && ~isempty(getenv('DISPLAY')) % plotting require a display, and Java Virtual Machine to call `figure;`
    		                        plot_nonuni_kernel(w_new,pyr_tt(s,:),1,0,0,[],1);
    		                    else
    		                        w_vis = plotFiltersEFF(makeEFF(size(pyr_deblurred{s}),theta_s(:,w_new(:)~=0),[6 8],Kblur_s),w_new(w_new(:)~=0));
    		                        imagesc(w_vis); 
    		                        axis off; axis image; camproj('orthographic'); view(0,90); title('Kernel');
    		                    end
    		                else 
    		                    if synthetic
    		                        w_vis = cat(3,zeros(size(w_true_s)),w_true_s/max(max(w_true_s(:)),1e-10),reshape(w_new/max(max(w_new(:)),1e-10),size(pyr_tt{s,1})));
    		                    else
    		                        w_vis = repmat(reshape(w_new/max(max(w_new(:)),1e-10),size(pyr_tt{s,1})),[1 1 3]);
    		                    end
    		                    imagesc(w_vis);
    		                    axis off; axis image;
    		                    title('Kernel');
    		                end
    		                drawnow
    		            end
    		            if final_iter || (save_intermediate_images==2) || (final_iter_this_level && save_intermediate_images==1)
    		                if non_uniform
    		                    if usejava('jvm') && ~isempty(getenv('DISPLAY')) % plotting require a display, and Java Virtual Machine to call `figure;`
    		                        plot_nonuni_kernel_write(w_new,pyr_tt(s,:),fullfile(output_directory,filename_kernel(s,iter,'kernel')));
    		                    end
    		                    imwrite(plotFiltersEFF(makeEFF(size(pyr_deblurred{s}),thetagrid2list(pyr_tt(s,:),w_new(:)~=0),[6 8],Kblur_s),w_new(w_new(:)~=0)),fullfile(output_directory,filename_kernel(s,iter,'filters')));
    		                else
    		                    imwrite(reshape(w_new,size(pyr_tt{s,1}))/max(w_new(:)),fullfile(output_directory,filename_kernel(s,iter,'kernel')));
    		                end
    		            end
    		            fprintf(logfile,'%.1fs\n',toc(wtime));
    		        end
    		        if iter == num_iters(s)
    		            fprintf(logfile,'\t=========\n\tPSF estimation took: %.1fs at scale %d (%d x %d)\n\t=========\n',toc(totaltime),s,hL,wL);
    		        end
    		        % Deconvolution =========================================================
    		        fprintf(logfile,'\tDeconvolution... ');
    		        L_new = zeros(size(L_old));
    		        if do_colour_this_iter, LC_new = zeros(hL,wL,3); end
    		        % Do gray deconvolution or colour deconvolution? c=0 is gray, c=1 is colour
    		        if ~do_estimate_kernel && do_colour_this_iter
    		            c0 = 1; % don't bother with grayscale deconvolution
    		        else
    		            c0 = 0; % do grayscale deconvolution
    		        end
    		        if ~iscell(image_method_this_iter)
    		            image_method_this_iter = {image_method_this_iter};
    		        end
    		        if do_colour_this_iter
    		            tmpB = BC;      satmask = pyr_saturation_mask_colour{s};
    		        else
    		            tmpB = B;       satmask = pyr_saturation_mask{s};
    		        end
    		        if final_iter, satmask(:,:,:) = 1; end
    		        for meth = image_method_this_iter
    		            deconvtime=tic;
    		            switch meth{1}
    		            case {'conjgrad','conjgrad_eff'}
    		                % Run deblurring
    		                fprintf(logfile,'conjugate gradients deblurring... ');
    		                if non_uniform
    		                    l2deconv_eff = false;
    		                    if strfind(meth{1},'eff')
    		                        l2deconv_eff = true;
    		                    end
    		                    [L_deblur, pad] = deconvL2NonUni_w_gradData(tmpB,w_new,alpha,deconv_maxit_this_iter,1,1,0,0,0,non_uniform,satmask,[],[omega0,omega1,omega2],theta_s,Kblur_s,l2deconv_eff);
    		                else
    		                    [L_deblur, pad] = deconvL2NonUni_w_gradData(tmpB,w_new,alpha,deconv_maxit_this_iter,1,1,0,0,0,non_uniform,satmask,[],[omega0,omega1,omega2]);
    		                end                    
    		                L_deblur = padImage(L_deblur, -pad);
    		            case 'krishnan'
    		                fprintf(logfile,'krishnan & fergus deblurring... ');
    		                if non_uniform
    		                    L_deblur = fast_deconv_eff(tmpB, w_new(w_new~=0), kf_lambda, kf_exponent, non_uniform, theta_s(:,w_new~=0), Kblur_s);
    		                else
    		                    L_deblur = fast_deconv(tmpB, w_new, kf_lambda, kf_exponent);
    		                end
    		            case 'direct'
    		                fprintf(logfile,'direct deblurring... ');
    		                if update_saturation_mask && c==c0
    		                    % Deblur once with no mask
    		                    w_new = w_new/sum(w_new(:));
    		                    if non_uniform
    		                        L_direct = deconvDirectEFF(tmpB,w_new,non_uniform,theta_s,Kblur_s,alpha);
    		                    else
    		                        L_direct = deconvDirectEFF(tmpB,w_new,non_uniform,alpha);
    		                    end
    		                    % Find saturated pixels
    		                    sharp_saturation_mask = sum(L_direct < 1, 3) == size(L_direct,3);
    		                    % Propagate mask to affected pixels in blurry image
    		                    if non_uniform
    		                        satmask = blurEFF(double(sharp_saturation_mask), w_new, theta_s, Kblur_s);
    		                    else
    		                        satmask = ifft2(fft2(double(sharp_saturation_mask)).*psf2otf(w_new,size(sharp_saturation_mask)),'symmetric');
    		                    end
    		                    satmask = imerode(satmask > 1-2*min(w_new(w_new(:)>0)), ones(5));
    		                    pyr_saturation_mask_colour{s} = repmat(satmask, [1 1 3]);
    		                    pyr_saturation_mask{s} = satmask;
    		                    % Deblur again
    		                    if non_uniform
    		                        L_deblur = deconvDirectEFF(tmpB,w_new,non_uniform,theta_s,Kblur_s,alpha,[],sum(satmask,3)>0);
    		                    else
    		                        L_deblur = deconvDirectEFF(tmpB,w_new,non_uniform,alpha,sum(satmask,3)>0);
    		                    end
    		                else
    		                    % Just deblur once with the mask
    		                    if non_uniform
    		                        L_deblur = deconvDirectEFF(tmpB,w_new,non_uniform,theta_s,Kblur_s,alpha,[],sum(satmask,3)>0);
    		                    else
    		                        L_deblur = deconvDirectEFF(tmpB,w_new,non_uniform,alpha,sum(satmask,3)>0);
    		                    end
    		                end
    		                % if non_uniform
    		                %     L_deblur = deconvDirectEFF(tmpB,w_new,non_uniform,theta_s,Kblur_s,alpha,[],sum(satmask,3)>0);
    		                % else
    		                %     L_deblur = deconvDirectEFF(tmpB,w_new,non_uniform,alpha,sum(satmask,3)>0);
    		                % end
    		            case {'rl','rl_eff','combinedrl','combinedrl_eff'}
    		                % Do the deblurring
    		                fprintf(logfile,'richardson-lucy deblurring... ');
    		                RLargs = {};
    		                if non_uniform
    		                    RLargs{1} = theta_s;
    		                    RLargs{2} = Kblur_s;
    		                    % Use Efficient Filter Flow to approximate the non-uniform blur when deblurring?
    		                    if strfind(meth{1},'eff')
    		                        RLargs = cat(2,RLargs,'eff');
    		                    end
    		                end
    		                RLargs = cat(2,RLargs,'num_iters',deconv_maxit_this_iter);
    		                % Handle saturation?
    		                if strfind(meth{1},'combinedrl')
    		                    RLargs = cat(2,RLargs,'forward_saturation','prevent_ringing');
    		                end
    		                L_deblur = deconvRL(tmpB,w_new,non_uniform,RLargs{:});
    		            case {'sparse','sparse_eff'}
    		                % Run deblurring
    		                fprintf(logfile,'sparse deblurring... ');
    		                if non_uniform
    		                    spsdeconv_eff = false;
    		                    if strfind(meth{1},'eff')
    		                        spsdeconv_eff = true;
    		                    end
    		                    L_deblur = deconvSpsNonUni(tmpB,w_new,alpha,deconv_maxit_this_iter,non_uniform,satmask,theta_s,Kblur_s,spsdeconv_eff);
    		                else
    		                    L_deblur = deconvSpsNonUni(tmpB,w_new,alpha,deconv_maxit_this_iter,non_uniform,satmask);
    		                    % L_old = zeros(size(tmpB));
    		                    % for chan=1:size(tmpB,3)
    		                    %     L_old(:,:,chan) = deconvSps(tmpB(:,:,chan),w_new,alpha,deconv_maxit_this_iter);
    		                    % end
    		                end
    		            otherwise
    		                error('Unknown deconvolution method')
    		            end
    		            fprintf(logfile,'%.1fs\n',toc(deconvtime));
    		            % Collect deblurred image as current estimate of latent image
    		            if do_colour_this_iter
    		                LC_new = L_deblur;
    		                L_new = rgb2gray_local(LC_new);
    		            else
    		                L_new = L_deblur;
    		            end
    		            % Write deblurred image to file
    		            if final_iter || (save_intermediate_images==2) || (final_iter_this_level && save_intermediate_images==1)
    		                imwrite(respfn_disp(L_deblur,resp_params),fullfile(output_directory,filename_image(L_deblur,s,iter,['deblurred_' meth{1}])),'jpg','Quality',100);
    		            end
    		            if final_iter && strcmp(meth{1},image_method_this_iter{end}) && exist('filename_final_image','var')
    		                if exist(fullfile(output_directory,filename_intermediate_image),'file')
    		                    delete(fullfile(output_directory,filename_intermediate_image));
    		                end
    		                imwrite(respfn_disp(L_deblur,resp_params),fullfile(output_directory,filename_final_image),'jpg','Quality',100);
    		            elseif exist('filename_intermediate_image','var')
    		                if exist(fullfile(output_directory,filename_final_image),'file')
    		                    delete(fullfile(output_directory,filename_final_image));
    		                end
    		                imwrite(respfn_disp(L_deblur,resp_params),fullfile(output_directory,filename_intermediate_image),'jpg','Quality',100);
    		            end
    		        end
    		        if do_display
    		            if do_colour_this_iter
    		                subplot(235); imagesc(respfn_disp(BC+cat(3,~M,zeros(hB,wB,2)),resp_params)); colormap gray; axis image; axis off; title('Blurry')
    		                subplot(236); imagesc(respfn_disp(LC_new,resp_params)); colormap gray; axis image; axis off; caxis([0 1]); title('Latent')
    		            else
    		                subplot(235); imagesc(respfn_disp(cat(3,B+~M,B,B),resp_params)); colormap gray; axis image; axis off; title('Blurry')
    		                subplot(236); imagesc(respfn_disp(L_new,resp_params)); colormap gray; axis image; axis off; caxis([0 1]); title('Latent')
    		            end
    		            drawnow
    		        end
    		        if exist('im_true','var') && (final_iter || save_intermediate_images>0)
    		            imwrite(respfn_disp(T,resp_params),fullfile(output_directory,filename_image(T,s,iter,'true')),'jpg','Quality',100);
    		        end
    		    end % for iter = 1:num_iters(s)
    		    % Store results for this scale
    		    if do_deblur(s)
    		        pyr_deblurred{s} = respfn(L_new,resp_params);
    		        pyr_deblurred_colour{s} = respfn(LC_new,resp_params);
    		        if any(isnan(pyr_deblurred_colour{s})), error('NaN in pyr_deblurred_colour{s}!'); end
    		        if any(isnan(pyr_deblurred{s})), error('NaN in pyr_deblurred{s}!'); end
    		    end
    		    if do_estimate_kernel
    		        pyr_kernel{s} = reshape(w_new,size(pyr_tt{s,1}));
    		    end
    		    % Upsample
    		    if s > final_level
    		        fprintf(logfile,'\tUpsampling... ');
    		        % Upsample image =========================================
    		        if do_deblur(s-1)
    		            pyr_deblurred{s-1} = imresizefn(pyr_deblurred{s},size(pyr_deblurred{s-1}(:,:,1)),'bicubic');
    		            pyr_deblurred_colour{s-1} = imresizefn(pyr_deblurred_colour{s},size(pyr_deblurred_colour{s-1}(:,:,1)),'bicubic');
    		        end
    		        % Upsample kernel =========================================
    		        if do_estimate_kernel
    		            if non_uniform
    		                pyr_kernel{s-1} = upsample_kernel_map(pyr_kernel{s},pyr_tt(s,:),pyr_tt(s-1,:));
    		            else
    		                pyr_kernel{s-1} = upsample_kernel_map(pyr_kernel{s},pyr_tt(s,:),pyr_tt(s-1,:),scale_ratio_k);
    		            end
    		        end
    		        fprintf(logfile,'done.\n')
    		    end
    		    if save_mat && (final_iter || save_intermediate_pyramids)
    		        fprintf(logfile,'Saving... ')
    		        save(fullfile(output_directory,'results.mat'),'pyr_deblurred','pyr_deblurred_colour','pyr_kernel','resp_params','-append');
    		        fprintf(logfile,'done.\n')
    		    end
    		end
    		
    		fprintf(logfile,'Total time: %.1fs\n',toc(totaltime));
    		*/	
    	}
}