package gov.nih.mipav.model.algorithms;


import java.io.IOException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.*;

/**
The MIT License (MIT)
Copyright (c) 2015 Eugeniy Sokol

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), 
to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute,
sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 
*/

//Split-Radix Fast Hartley Transform
//Inplace with power of 2 data length
//Author: 2015-2016 Eugeniy Sokol, Magnitogorsk
// Ported from C++ to Java by William Gandler


public class HartleyTransform extends AlgorithmBase {
    private ModelImage transformImage;
    private ModelImage inverseImage;
    
    private static final double pi =    3.14159265358979323846264338327950;
    private static final double pi4 =   0.78539816339744830961566084581988;
    private static final double sqrt2 = 1.41421356237309504880168872420970;
    private static final double cos45 = 0.70710678118654752440084436210485;
    private static final double cos22 = 0.92387953251128675612818318939679;
    private static final double sin22 = 0.38268343236508977172845998403040;
    
    private int fht_instance_count[]=  new int[]{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
    private double fht_trig_tables[][] = new double[32][];
    private int fht_revbin_tables[][] = new int[32][];
    private int fht_revbin_counts[]=  new int[]{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

    private int size;
    private int ldn;
    
    public HartleyTransform() {
		
	}
    
    public HartleyTransform(int size)
    {
    	this.size = size;
    	ldn =(int)(Math.log((double)size)/Math.log(2.));
    	fht_instance_count[ldn]++;

    	int i, j, index, k;

    	// trigonometry table
    	k=2;
    	for(int cnt=1;cnt<=size/4;cnt*=2)
    	{
    		if(fht_trig_tables[k]==null)
    		{
    			double trig_table[] = fht_trig_tables[k] = new double[cnt*4];
    			index = 0;
    			double nn = cnt*4;
    			for (i = 1; i <= cnt; i++)
    			{
    				trig_table[index + 0] = Math.sin(i * pi / nn);
    				trig_table[index + 1] = Math.cos(i * pi / nn);

    				trig_table[index + 2] = Math.sin(3.*i * pi / nn);
    				trig_table[index + 3] = Math.cos(3.*i * pi / nn);

    				index+=4;
    			}
    		}
    		k++;
    	}

    	// permute table
    	if(fht_revbin_tables[ldn]==null)
    	{
    		// count of non-symmetric numbers
    		k=0;
    		if(ldn%2==0)
    			k=(int)(Math.pow(2,ldn-1)-Math.pow(2,ldn/2-1));
    		else
    			k=(int)(Math.pow(2,ldn-1)-Math.pow(2,(ldn+1)/2-1));

    		fht_revbin_counts[ldn] = k;
    		fht_revbin_tables[ldn] = new int[k*2];
    		index = 0;
    		for (int x = 1; x < size; x++)
    		{
    			int t=x, r=0;
    			for(i=0;i<ldn;i++)
    			{
    				r<<=1;
    				r+=t&1;
    				t>>=1; 
    			}
    			if(r>x)
    			{
    				fht_revbin_tables[ldn][index]=x;
    				fht_revbin_tables[ldn][index+1]=r;
    				index+=2;
    			}
    		}
    	}
    }
    
    public HartleyTransform(ModelImage transformImage, ModelImage inverseImage, ModelImage srcImg) {
		super(null, srcImg);
		this.transformImage = transformImage;
		this.inverseImage = inverseImage;
	}
	
	public void finalize() {
		fht_instance_count[ldn]--;
		if(fht_instance_count[ldn]==0)
		{
			fht_revbin_tables[ldn]=null;
		}
		int k0 = 31;
		while(k0>0 && fht_instance_count[k0]==0)
			k0--;
		k0+=1;
		for(int k=k0;k<32;k++)
		{
			if(fht_trig_tables[k]!=null)
			{
				fht_trig_tables[k] = null;
			}
		}
	
	}
	
	public void show(double data[], int size)
	{
		for(int i=0;i<size;i++)
			System.out.println(" i = " + i + "  " + Math.floor(data[i]*1000.+.5)/1000.);
	}
	
	public void testHartley()
	{
		double fir[]= new double[]{11,22,33,0,0,0,0,0, 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0};
		double data[]= new double[]{1,0,1,0,0,0,3,0,    0,0,0,0,0,0,0,0, 10,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0};

		int size = 32;
		HartleyTransform fht = new HartleyTransform(size);
		//HartleyTransform fht2 = new HartleyTransform(size);
		//HartleyTransform fht3 = new HartleyTransform(size/2);
		//std::cout.precision(3);
		
		// convolution
		//System.out.println("Convolution test using FHT.");
		//System.out.println("fir:");
		//show(fir, size);
		//System.out.println("source");
		//show(data, size);
		//	
		//fht.transform(fir, false);
		fht.transform(data, true);
		fht.back_transform(data);
		for (int i = 0; i < 32; i++) {
			System.out.println("data["+i+"] = " + data[i]);
		}
		/*fht.hartley_multiply(data, fir, false);
		fht.back_transform(data);
		//
		System.out.println("result of convolution:");
		show(data, size);
		
		// spectrum
		System.out.println("spectrum of fir:");
		for(int i=0;i<=size/2;i++) {
			double com[] = (fht.get_frequency(fir, i));
			double ans = zabs(com[0], com[1]);
			System.out.println("i = " + i + "  " + ans);
		}*/

		return;
	}
	
	public void transform(double data[], boolean scaled)
	{
		step_addsub(data, ldn);
		if (scaled)
			for (int i = 0; i < size; i++) 
				data[i] *= 1.0/(double)size;
		revbin_permute(data);
	}
	
	public void back_transform(double fht_data[])
	{
		step_addsub(fht_data, ldn);
		revbin_permute(fht_data);
	}
	
	public void hartley_multiply(double fht_data[], double fht_fir[], boolean scaled)
	{
		double mulf = 0.5*1.0;
		double mul0 = 1.0;
		if(scaled)
		{
			mulf*=size;
			mul0*=size;
		}
		int i = 1, j = size-1;
		do
		{
			double yp = (fht_data[i] + fht_data[j]) * mulf;
			double ym = (fht_data[i] - fht_data[j]) * mulf;
			fht_data[i] = (fht_fir[i] * yp + fht_fir[j] * ym);
			fht_data[j] = (fht_fir[j] * yp - fht_fir[i] * ym);
			i++; j--;
		}
		while (i<j);

		fht_data[0] *= fht_fir[0] * mul0;
		fht_data[size/2] *= fht_fir[size/2] * mul0;
	}

	public double[] get_frequency(double[] fht_data, int number)
	{
		double com[] = new double[2];
		if (number==0)  {
			com[0] = fht_data[0];
			com[1] = 0.0;
		}
		else {
			if (number==size/2) {
				com[0] = fht_data[size/2];
				com[1] = 0.0;
			}
			else
			{
				double a = 0.5*fht_data[number];
				double b = 0.5*fht_data[size-number];
				com[0] = a+b;
				com[1] = a-b;
			}
	    }
		return com;
	}

	
	private void revbin_permute(double data[])
	{
		int rev_count = fht_revbin_counts[ldn];
		for (int i = 0; i <rev_count; i++)
		{
			int x = fht_revbin_tables[ldn][i*2];
			int r = fht_revbin_tables[ldn][i*2+1];
			double vx = data[x];
			double vr = data[r];
			data[r] = vx;
			data[x] = vr;
		}
	}
	
	private void step_addsub(double data[], int ldn)
	{
		double datan[];
		int j;
		double data4[] = new double[4];
		double data8[] = new double[8];
		double data5[] = new double[1];
		double data7[] = new double[1];
		double data0[] = new double[1];
		double data1[] = new double[1];
		if(ldn==4)
		{
			for(int i=0;i<4;i++) 
			{
				double a = data[i];
				double b = data[i+4];
				double c = data[i+8];
				double d = data[i+12];

				double ac=a+c;
				double amc=a-c;
				double bd=b+d;
				double bmd=b-d;

				data[i]    =ac+bd;
				data[i+4]  =ac-bd;
				data[i+8]  =amc;
				data[i+12] =bmd;		
			}

			data5[0] = data[5];
			data7[0] = data[7];
			addsub(data5,data7,cos45);
			data[5] = data5[0];
			data[7] = data7[0];

			step_addsub4(data);
			for (j = 0; j < 4; j++) {
				data4[j] = data[j + 4];
			}
			step_addsub4(data4);
			for (j = 0; j < 4; j++) {
				data[j + 4] = data4[j];
			}

			for (j = 0; j < 8; j++) {
				data8[j] = data[j+8];
			}
			step_rotate8(data8);
			for (j = 0; j < 8; j++) {
				data[j+8] = data8[j];
			}

			for (j = 0; j < 4; j++) {
				data4[j] = data[j + 8];
			}
			step_addsub4(data4);
			for (j = 0; j < 4; j++) {
				data[j + 8] = data4[j];
			}
			for (j = 0; j < 4; j++) {
				data4[j] = data[j + 12];
			}
			step_addsub4(data4);
			for (j = 0; j < 4; j++) {
				data[j + 12] = data4[j];
			}
			return;
		}

		if(ldn==3)
		{
			step_addsub8(data);

			step_addsub4(data);
			for (j = 0; j < 4; j++) {
				data4[j] = data[j + 4];
			}
			step_addsub4(data4);
			for (j = 0; j < 4; j++) {
				data[j + 4] = data4[j];
			}
			return;
		}

		if(ldn==2)
		{
			step_addsub4(data);
			return;
		}

		if(ldn==1)
		{
			data0[0] = data[0];
			data1[0] = data[1];
			addsub(data0,data1);
			data[0] = data0[0];
			data[1] = data1[0];
			return;
		}

		//---------------------------
		int n = (int)Math.round(Math.pow(2,ldn-1));
		int nh = n/2;

		for(int i=0;i<nh;i++) 
		{
			double a = data[i];
			double b = data[i+nh];
			double c = data[i+n];
			double d = data[i+n+nh];

			double ac=a+c;
			double amc=a-c;
			double bd=b+d;
			double bmd=b-d;

			data[i]=ac+bd;
			data[i+nh]=ac-bd;
			data[i+n]=amc;
			data[i+n+nh]=bmd;		
		}
		// recursion calls
		step_addsub(data,    ldn-2);
		datan = new double[data.length - nh];
		for (j = 0; j < data.length - nh; j++) {
			datan[j] = data[nh + j];
		}
		step_rotate(datan, ldn-2);
		for (j = 0; j < data.length - nh; j++) {
			data[nh + j] = datan[j];
		}
		datan = new double[data.length - n];
		for (j = 0; j < data.length - n; j++) {
			datan[j] = data[n + j];
		}
		step_rotate(datan,  ldn-1);
		for (j = 0; j < data.length - n; j++) {
			data[n + j] = datan[j];
		}
	}

	
	private void step_rotate(double data[], int ldn)
	{
		double data0[] = new double[1];
		double datan[];
		int j;
		int n = (int)Math.round(Math.pow(2,ldn-1));
		int nh = n/2;

		data0[0] = data[0];
		datan = new double[1];
		datan[0] = data[n];
		addsub(data0, datan);
		data[0] = data0[0];
		data[n] = datan[0];
		data[nh]*= sqrt2;
		data[n+nh]*= sqrt2;
        int offset = 0;

		int index = 1;
		int step = n-2;
		while(step>0)
		{
			double a = data[index];
			double b = data[index+step];
			double c = data[index+n];
			double d = data[index+n+step];

			double ab = a+b;
			double amb = a-b;
			double cd = c+d;
			double cmd = c-d;

			double ss = fht_trig_tables[ldn][offset];
			double cc = fht_trig_tables[ldn][offset+1];
			data[index] =      ab * cc - cmd * ss;
			data[index+step] = ab * ss + cmd * cc;

			double ss3 = fht_trig_tables[ldn][offset+2];
			double cc3 = fht_trig_tables[ldn][offset+3];
			data[index+n] =      amb * cc3 + cd * ss3;
			data[index+n+step] = amb * ss3 - cd * cc3;

			offset+=4;
			index++;
			step-=2;
		}
		// recursion calls
		step_addsub(data,   ldn-1);
		datan = new double[data.length - n];
		for (j = 0; j < data.length - n; j++) {
			datan[j] = data[n + j];
		}
		step_addsub(datan, ldn-1);
		for (j = 0; j < data.length - n; j++) {
			data[n + j] = datan[j];
		}
	}

	private void step_addsub8(double data[])
	{
		double dataa[] = new double[1];
		double datab[] = new double[1];
		dataa[0] = data[0];
		datab[0] = data[4];
		addsub(dataa,datab);
		data[0] = dataa[0];
		data[4] = datab[0];
		dataa[0] = data[1];
		datab[0] = data[5];
		addsub(dataa,datab);
		data[1] = dataa[0];
		data[5] = datab[0];
		dataa[0] = data[2];
		datab[0] = data[6];
		addsub(dataa,datab);
		data[2] = dataa[0];
		data[6] = datab[0];
		dataa[0] = data[3];
		datab[0] = data[7];
		addsub(dataa,datab);
		data[3] = dataa[0];
		dataa[0] = data[5];
		addsub(dataa,datab,cos45);
		data[5] = dataa[0];
		data[7] = datab[0];
	}

	private void step_rotate8(double data[])
	{   
		double data0[] = new double[1];
		double data4[] = new double[1];
		data0[0] = data[0];
		data4[0] = data[4];
		addsub(data0, data4);
		data[0] = data0[0];
		data[4] = data4[0];
		data[2]*= sqrt2;
		data[6]*= sqrt2;

		double a = data[1];
		double b = data[3];
		double c = data[5];
		double d = data[7];

		double ab = a+b;
		double amb = a-b;
		double cd = c+d;
		double cmd = c-d;

		data[1] = ab * cos22 - cmd * sin22;
		data[3] = ab * sin22 + cmd * cos22;
		data[5] = cd * cos22 + amb * sin22;
		data[7] = amb * cos22 - cd * sin22;
	}
	
	private void step_addsub4(double data[])
	{
		double v02= data[0]+data[2];
		double v13= data[1]+data[3];
		double v02m=data[0]-data[2];
		double v13m=data[1]-data[3];
		data[0]= v02+v13;
		data[1]= v02-v13;
		data[2]=v02m+v13m;
		data[3]=v02m-v13m;
	}
	
	private void addsub(double u[], double v[])
	{
		double tempu = u[0];
		u[0] = u[0] + v[0];
		v[0] = tempu - v[0];
	}

	private void addsub(double u[], double v[], double scale)
	{
		double tempu = u[0];
		u[0] = (u[0] + v[0])*scale;
		v[0] = (tempu - v[0])*scale;
	}
	
	private void rotate(double u[], double v[], double c, double s)
	{
		double tempu = u[0];
		u[0] = u[0]*c + v[0]*s;
		v[0] = tempu*s - v[0]*c;
	}

    /**
     * zabs computes the absolute value or magnitude of a double precision complex variable zr + j*zi.
     * 
     * @param zr double
     * @param zi double
     * 
     * @return double
     */
    private double zabs(final double zr, final double zi) {
        double u, v, q, s;
        u = Math.abs(zr);
        v = Math.abs(zi);
        s = u + v;

        // s * 1.0 makes an unnormalized underflow on CDC machines into a true
        // floating zero
        s = s * 1.0;

        if (s == 0.0) {
            return 0.0;
        } else if (u > v) {
            q = v / u;

            return (u * Math.sqrt(1.0 + (q * q)));
        } else {
            q = u / v;

            return (v * Math.sqrt(1.0 + (q * q)));
        }
    }

	public void runAlgorithm() {
		int xDim;
		int yDim;
		int zDim;
		int length;
		double doubleBuffer[];
		int xTest;
		int yTest;
		int z;
		double src[][];
        double dst[][];
        int x;
        int y;
		xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        length = xDim * yDim;
        doubleBuffer = new double[length];
        zDim = 1;
        if (srcImage.getNDims() > 2) {
        	zDim = srcImage.getExtents()[2];
        }
         
        xTest = xDim;
        while ((xTest % 2) == 0) {
        	xTest = xTest/2;
        }
        if (xTest != 1) {
        	MipavUtil.displayError("X dimension not a power of 2");
        	setCompleted(false);
        	return;	
        }
        yTest = yDim;
        while ((yTest % 2) == 0) {
        	yTest = yTest/2;
        }
        if (yTest != 1) {
        	MipavUtil.displayError("Y dimension not a power of 2");
        	setCompleted(false);
        	return;	
        }
        src = new double[yDim][xDim];
        dst = new double[yDim][xDim];
        for (z = 0; z < zDim; z++) {
        	try {
                srcImage.exportData(z * length, length, doubleBuffer); // locks and releases lock
            } catch (IOException error) {
                doubleBuffer = null;
                errorCleanUp("Hartley Transform: Image(s) locked", true);

                return;
            }
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			src[y][x] = doubleBuffer[x + y * xDim];
        			dst[y][x] = 0;
        		}
        	}
        	
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			doubleBuffer[x + y * xDim] = dst[y][x];
        			src[y][x] = 0;
        		}
        	}
        	try {
                transformImage.importData(z*length, doubleBuffer, false);
             } catch (IOException error) {
                doubleBuffer = null;
                errorCleanUp("Discrete Sine Transform: Image(s) locked", true);

                return;
             }
        	// Inverse transform
            
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			doubleBuffer[x + y * xDim] = src[y][x];
        		}
        	}
        	try {
                inverseImage.importData(z*length, doubleBuffer, false);
             } catch (IOException error) {
                doubleBuffer = null;
                errorCleanUp("Hartley Transform: Image(s) locked", true);

                return;
             }
        }
        transformImage.calcMinMax();
        inverseImage.calcMinMax();
        setCompleted(true);
        return;
	}
	
	

}

