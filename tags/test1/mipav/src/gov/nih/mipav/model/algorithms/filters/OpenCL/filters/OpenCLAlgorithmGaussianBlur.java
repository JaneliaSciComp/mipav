package gov.nih.mipav.model.algorithms.filters.OpenCL.filters;


import static org.jocl.CL.CL_DEVICE_MAX_MEM_ALLOC_SIZE;
import static org.jocl.CL.CL_MEM_COPY_HOST_PTR;
import static org.jocl.CL.CL_TRUE;
import static org.jocl.CL.clBuildProgram;
import static org.jocl.CL.clCreateBuffer;
import static org.jocl.CL.clCreateCommandQueue;
import static org.jocl.CL.clCreateKernel;
import static org.jocl.CL.clCreateProgramWithSource;
import static org.jocl.CL.clEnqueueNDRangeKernel;
import static org.jocl.CL.clEnqueueReadBuffer;
import static org.jocl.CL.clFinish;
import static org.jocl.CL.clReleaseMemObject;
import static org.jocl.CL.clSetKernelArg;
import static org.jocl.CL.stringFor_errorCode;
import gov.nih.mipav.model.GaussianKernelFactory;
import gov.nih.mipav.model.Kernel;
import gov.nih.mipav.model.algorithms.GenerateGaussian;
import gov.nih.mipav.model.algorithms.OpenCLAlgorithmBase;
import gov.nih.mipav.model.structures.ModelImage;

import java.io.IOException;

import org.jocl.CL;
import org.jocl.Pointer;
import org.jocl.Sizeof;
import org.jocl.cl_command_queue;
import org.jocl.cl_kernel;
import org.jocl.cl_mem;
import org.jocl.cl_program;

import gov.nih.mipav.view.ViewJFrameImage;


/**
 * OpenCL Algorithm computes the GaussianBlur of a 2D, 3D or 4D image.
 */
public class OpenCLAlgorithmGaussianBlur extends OpenCLAlgorithmBase {


    /** Storage location of the 2D Gaussian kernel. */
    private float[] GaussData_2D;
    private float[] GaussData_3D;

    /** Extents of the 2D kernel. */
    private int[] kExtents_2D;

    /** Origins of the 2D kernel. */
    private int[] kOrigins_2D;

    /** Extents of the 3D kernel. */
    private int[] kExtents_3D;

    /** Origins of the 3D kernel. */
    private int[] kOrigins_3D;

    /** Color flags. */
    private int[] colorMask = new int[]{0,0,0,0};

    /** Standard deviations of the Gaussian used to calculate the kernels. */
    private float[] sigmas;


    /**
     * Creates a new AlgorithmGaussianBlur object.
     *
     * @param  srcImg    the source image
     * @param  sigmas    the standard deviations of the Gaussian
     * @param  maskFlag  VOI Masking
     * @param  img25D    calculate per slice or for 3D volume
     */
    public OpenCLAlgorithmGaussianBlur(ModelImage srcImg, float[] sigmas, boolean maskFlag, boolean separable, boolean img25D) {
        this(null, srcImg, sigmas, maskFlag, separable, img25D);
    }

    /**
     * Constructor which sets the source and destination images, the minimum and maximum progress value.
     *
     * @param  destImg   the destination image
     * @param  srcImg    the source image
     * @param  sigmas    the sigmas
     * @param  maskFlag  the mask flag
     * @param  img25D    the 2.5D indicator
     */
    public OpenCLAlgorithmGaussianBlur(ModelImage destImg, ModelImage srcImg, float[] sigmas, boolean maskFlag, boolean separable,
                                 boolean img25D) {
        this(destImg, srcImg, sigmas, maskFlag, separable, img25D, CL.CL_DEVICE_TYPE_GPU);
    }

    /**
     * Constructor which sets the source and destination images, the minimum and maximum progress value.
     *
     * @param  destImg   the destination image
     * @param  srcImg    the source image
     * @param  sigmas    the sigmas
     * @param  maskFlag  the mask flag
     * @param  img25D    the 2.5D indicator
     */
    public OpenCLAlgorithmGaussianBlur(ModelImage destImg, ModelImage srcImg, float[] sigmas, boolean maskFlag, boolean separable,
                                 boolean img25D, long deviceType) {
        super(destImg, srcImg, maskFlag, deviceType);

        this.sigmas = sigmas;
		this.separable = separable;
		this.image25D = img25D;
    }

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
    	GaussData_2D = null;
    	GaussData_3D = null;
        destImage = null;
        srcImage = null;
    	colorMask = null;
        kOrigins_2D = null;
        kExtents_2D = null;
        kOrigins_3D = null;
        kExtents_3D = null;
        sigmas = null;
        super.finalize();
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        if ( !separable )
        {
        	if (srcImage.getNDims() == 2) {
        		makeKernels2D();
        	} else if ((srcImage.getNDims() == 3) && (image25D == false)) {
        		makeKernels2D();
        		makeKernels3D();
        	} else if ((srcImage.getNDims() == 3) && (image25D == true)) {
        		makeKernels2D();
        	} else if (srcImage.getNDims() == 4) {
        		makeKernels2D();
        		makeKernels3D();
        	}
		}

        if (srcImage.getNDims() == 2) {
        	if ( !separable )
			{
				gaussianBlur25D();
			}
        	else
        	{
				gaussianBlurSep25D();
        	}
        } else if ((srcImage.getNDims() == 3) && (image25D == false)) {
        	if ( !separable )
			{
				gaussianBlur3D(0);
			}
        	else
        	{
				gaussianBlurSep3D(0);
        	}
        } else if ((srcImage.getNDims() == 3) && (image25D == true)) {
        	if ( !separable )
			{
				gaussianBlur25D();
			}
        	else
        	{
				gaussianBlurSep25D();
        	}
        } else if (srcImage.getNDims() == 4) {
        	gaussianBlur4D();
        }		
        setCompleted(true);
    }


    /**
     * Creates 2D Gaussian kernels for the blurring process. The kernel size is always odd and proportional (8X) to the
     * standard deviation of the Gaussian.
     */
    private void makeKernels2D() {
        int xkDim, ykDim;
        int[] derivOrder = new int[2];

        kExtents_2D = new int[2];
        derivOrder[0] = 0;
        derivOrder[1] = 0;

        xkDim = Math.round(8 * sigmas[0]);

        if ((xkDim % 2) == 0) {
            xkDim++;
        }

        kExtents_2D[0] = xkDim;

        ykDim = Math.round(8 * sigmas[1]);

        if ((ykDim % 2) == 0) {
            ykDim++;
        }

        kExtents_2D[1] = ykDim;

        GaussData_2D = new float[xkDim * ykDim];

        GenerateGaussian Gauss = new GenerateGaussian(GaussData_2D, kExtents_2D, sigmas, derivOrder);
        Gauss.calc(false);
        Gauss.finalize();
        Gauss = null;

        kOrigins_2D = new int[2];
        kOrigins_2D[0] = (kExtents_2D[0]-1)>>1;
        kOrigins_2D[1] = (kExtents_2D[1]-1)>>1;
    }

    /**
     * Creates 3D Gaussian kernels for the blurring process. The kernel size is always odd and proportional (8X) to the
     * standard deviation of the Gaussian.
     */
    private void makeKernels3D() {
        int xkDim, ykDim, zkDim;
        int[] derivOrder = new int[3];

        kExtents_3D = new int[3];
        derivOrder[0] = 0;
        derivOrder[1] = 0;
        derivOrder[2] = 0;

        xkDim = Math.round(8 * sigmas[0]);
        // System.out.println("Sigma 0 = " + sigmas[0]);
        // System.out.println("Sigma 1 = " + sigmas[1]);
        // System.out.println("Sigma 2 = " + sigmas[2]);

        if ((xkDim % 2) == 0) {
            xkDim++;
        }

        kExtents_3D[0] = xkDim;

        ykDim = Math.round(8 * sigmas[1]);

        if ((ykDim % 2) == 0) {
            ykDim++;
        }

        kExtents_3D[1] = ykDim;

        zkDim = Math.round(8 * sigmas[2]);

        if ((zkDim % 2) == 0) {
            zkDim++;
        }

        kExtents_3D[2] = zkDim;

        GaussData_3D = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gauss = new GenerateGaussian(GaussData_3D, kExtents_3D, sigmas, derivOrder);
        Gauss.calc(false);
        Gauss.finalize();
        Gauss = null;
        
        kOrigins_3D = new int[3];
        kOrigins_3D[0] = (kExtents_3D[0]-1)>>1;
        kOrigins_3D[1] = (kExtents_3D[1]-1)>>1;
        kOrigins_3D[2] = (kExtents_3D[2]-1)>>1;
    }
    
	/**
	 * 2D Implementation: Create the OpenCL Buffers, Kernel, and CommandQueue. 
	 * Run the Kernel and save the results into a new image.
	 */
	private void gaussianBlur25D()
	{
		initCL(m_iDeviceType, null);
		
		int elementCount = width * height * depth * color;		
		float[] input = new float[ elementCount ];
		try {
			if ( this.entireImage )
			{
				srcImage.exportData( 0, input.length, input );
			}
			else
			{
				srcImage.exportDataUseMask( 0, input.length, input );
			}
		} catch (IOException e) {
			e.printStackTrace();
		}

		float[] output = new float[ elementCount ];

		int[] errcode = new int[1];
		cl_mem inputBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * input.length, Pointer.to(input), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		cl_mem outputBuffer = clCreateBuffer(cl, CL.CL_MEM_WRITE_ONLY,
				Sizeof.cl_float * output.length, null, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		cl_mem convolve = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * GaussData_2D.length, Pointer.to(GaussData_2D), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}
		
		int[] imageSize = new int[]{width, height};
		int[] maskSize = kExtents_2D;
		int[] maskOrigins = kOrigins_2D;
		
		// Read the program source code and create the program
		String source = readFile("Convolve.cl");
		cl_program program = clCreateProgramWithSource(cl, 1, 
				new String[]{ source }, null, null);
		clBuildProgram(program, 0, null, "-cl-mad-enable", null, null);
		String kernelName = (color == 1) ? "Convolve25D" : "Convolve25D_Color";
		cl_kernel kernel = clCreateKernel(program, kernelName, null);
		
		int arg = 0;
		clSetKernelArg(kernel, arg++, Sizeof.cl_mem, Pointer.to(inputBuffer));
		clSetKernelArg(kernel, arg++, Sizeof.cl_mem, Pointer.to(convolve));
		clSetKernelArg(kernel, arg++, Sizeof.cl_mem, Pointer.to(outputBuffer));
		clSetKernelArg(kernel, arg++, Sizeof.cl_int2, Pointer.to(imageSize));
		clSetKernelArg(kernel, arg++, Sizeof.cl_int2, Pointer.to(maskSize));
		clSetKernelArg(kernel, arg++, Sizeof.cl_int2, Pointer.to(maskOrigins));
		if ( color != 1 )
		{
			clSetKernelArg(kernel, arg++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
        clSetKernelArg(kernel, arg, Sizeof.cl_int, Pointer.to(new int[]{0}));
        int sliceArg = arg;

        long globalWorkSize[] = new long[]{width,height};        
        
		// create command queue:
		cl_command_queue commandQueue = 
				clCreateCommandQueue(cl, device, 0, null);
        for ( int i = 0; i < depth; i++ )
        {
            clSetKernelArg(kernel, sliceArg, Sizeof.cl_int, Pointer.to(new int[]{i}));
            clEnqueueNDRangeKernel(commandQueue, kernel, 2, null, globalWorkSize, null, 0, null, null);
		}
    	clEnqueueReadBuffer(commandQueue, outputBuffer, CL_TRUE, 0, Sizeof.cl_float * output.length, Pointer.to(output), 0, null, null);
		saveImage(output, 0, true );
		
		clReleaseMemObject(inputBuffer);
		clReleaseMemObject(outputBuffer);
	}
	
	private void gaussianBlurSep25D()
	{
		initCL(m_iDeviceType, null);
		
		int elementCount = width * height * depth * color;		
		float[] input = new float[ elementCount ];
		try {
			if ( this.entireImage )
			{
				srcImage.exportData( 0, input.length, input );
			}
			else
			{
				srcImage.exportDataUseMask( 0, input.length, input );
			}
		} catch (IOException e) {
			e.printStackTrace();
		}

		float[] output = new float[ elementCount ];

		int[] errcode = new int[1];
		cl_mem inputBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * input.length, Pointer.to(input), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		cl_mem outputBuffer = clCreateBuffer(cl, CL.CL_MEM_WRITE_ONLY,
				Sizeof.cl_float * output.length, null, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		// Convolve Seperable:
		GaussianKernelFactory gkf = GaussianKernelFactory.getInstance(sigmas);
		gkf.setKernelType(GaussianKernelFactory.BLUR_KERNEL);
		Kernel gaussianKernel = gkf.createKernel();
		OpenCLAlgorithmConvolver.convolveSep2D( cl, device, 
				inputBuffer, outputBuffer, width, height, depth, elementCount, gaussianKernel, 
				color, colorMask );

		// create command queue:
		cl_command_queue commandQueue = 
				clCreateCommandQueue(cl, device, 0, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}
    	clEnqueueReadBuffer(commandQueue, outputBuffer, CL_TRUE, 0, Sizeof.cl_float * output.length, Pointer.to(output), 0, null, null);
		saveImage(output, 0, true );
		
		clReleaseMemObject(inputBuffer);
		clReleaseMemObject(outputBuffer);
	}
	
	
	private void gaussianBlurSep25DSlices( )
	{
		// Convolve Seperable:
		GaussianKernelFactory gkf = GaussianKernelFactory.getInstance(sigmas);
		gkf.setKernelType(GaussianKernelFactory.BLUR_KERNEL);
		Kernel gaussianKernel = gkf.createKernel();

		int[] errcode = new int[1];

		// create command queue:
		cl_command_queue commandQueue = 
				clCreateCommandQueue(cl, device, 0, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}
		
		int elementCount = width * height * color;

		float[] output = new float[ elementCount ];
		cl_mem outputBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_WRITE,
				Sizeof.cl_float * output.length, null, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}
		

		float[] input = new float[ elementCount ];
		for ( int i = 0; i < depth; i++ )
		{
			try {
				if ( this.entireImage )
				{
					srcImage.exportData( i * input.length, input.length, input );
				}
				else
				{
					srcImage.exportDataUseMask( i * input.length, input.length, input );
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
			cl_mem inputBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR,
					Sizeof.cl_float * input.length, Pointer.to(input), errcode);
			if ( errcode[0] != CL.CL_SUCCESS )
			{
				System.err.println( stringFor_errorCode(errcode[0]) );
			}
			OpenCLAlgorithmConvolver.convolveSep2D( cl, device, 
					inputBuffer, outputBuffer, width, height, 1, elementCount, gaussianKernel, 
					color, colorMask );
			errcode[0] = clEnqueueReadBuffer(commandQueue, outputBuffer, CL_TRUE, 0, Sizeof.cl_float * output.length, Pointer.to(output), 0, null, null);
			if ( errcode[0] != CL.CL_SUCCESS )
			{
				System.err.println( stringFor_errorCode(errcode[0]) );
			}
			saveImage(output, i, (i == depth-1) );

			clReleaseMemObject(inputBuffer);
		}
		clReleaseMemObject(outputBuffer);
	}

	
	

	/**
	 * 3D Implementation: Create the OpenCL Buffers, Kernel, and CommandQueue. 
	 * Run the Kernel and save the results into a new image.
	 */    
	private void gaussianBlur3D( int time )
	{
		initCL(m_iDeviceType, null);
		int elementCount = width * height * depth * color;		
		
		float[] input = new float[ elementCount ];
		try {
			if ( this.entireImage )
			{
				srcImage.exportData( time * input.length, input.length, input );
			}
			else
			{
				srcImage.exportDataUseMask( time * input.length, input.length, input );
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	
		float[] output = new float[ elementCount ];
	
		int[] errcode = new int[1];
		cl_mem inputBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * input.length, Pointer.to(input), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}
	
		cl_mem outputBuffer = clCreateBuffer(cl, CL.CL_MEM_WRITE_ONLY,
				Sizeof.cl_float * output.length, null, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}
	
		cl_mem convolve_2D = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * GaussData_2D.length, Pointer.to(GaussData_2D), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}
	
		cl_mem convolve_3D = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * GaussData_3D.length, Pointer.to(GaussData_3D), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}
		
		int[] imageSize_2D = new int[]{width, height};
		int[] maskSize_2D = kExtents_2D;
		int[] maskOrigins_2D = kOrigins_2D;
		
		int[] imageSize_3D = new int[]{width, height, depth, 0};
		int[] maskSize_3D = new int[]{kExtents_3D[0], kExtents_3D[1], kExtents_3D[2], 0};
		int[] maskOrigins_3D = new int[]{kOrigins_3D[0], kOrigins_3D[1], kOrigins_3D[2], 0};
		
		// Read the program source code and create the program
		String source = readFile("Convolve.cl");
		cl_program program = clCreateProgramWithSource(cl, 1, 
				new String[]{ source }, null, null);
		clBuildProgram(program, 0, null, "-cl-mad-enable", null, null);
		
		// Set up 2D Kernel:
		String kernelName = (color == 1) ? "Convolve25D" : "Convolve25D_Color";
		cl_kernel kernel_2D = clCreateKernel(program, kernelName, null);
		
		int arg = 0;
		clSetKernelArg(kernel_2D, arg++, Sizeof.cl_mem, Pointer.to(inputBuffer));
		clSetKernelArg(kernel_2D, arg++, Sizeof.cl_mem, Pointer.to(convolve_2D));
		clSetKernelArg(kernel_2D, arg++, Sizeof.cl_mem, Pointer.to(outputBuffer));
		clSetKernelArg(kernel_2D, arg++, Sizeof.cl_int2, Pointer.to(imageSize_2D));
		clSetKernelArg(kernel_2D, arg++, Sizeof.cl_int2, Pointer.to(maskSize_2D));
		clSetKernelArg(kernel_2D, arg++, Sizeof.cl_int2, Pointer.to(maskOrigins_2D));
		if ( color != 1 )
		{
			clSetKernelArg(kernel_2D, arg++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
	    clSetKernelArg(kernel_2D, arg, Sizeof.cl_int, Pointer.to(new int[]{0}));
	    int sliceArg_2D = arg;
	    
	
	    
	    // Set up 3D Kernel:
		kernelName = (color == 1) ? "Convolve3D" : "Convolve3D_Color";
		cl_kernel kernel_3D = clCreateKernel(program, kernelName, null);
		
		arg = 0;
		clSetKernelArg(kernel_3D, arg++, Sizeof.cl_mem, Pointer.to(inputBuffer));
		clSetKernelArg(kernel_3D, arg++, Sizeof.cl_mem, Pointer.to(convolve_3D));
		clSetKernelArg(kernel_3D, arg++, Sizeof.cl_mem, Pointer.to(outputBuffer));
		clSetKernelArg(kernel_3D, arg++, Sizeof.cl_int4, Pointer.to(imageSize_3D));
		clSetKernelArg(kernel_3D, arg++, Sizeof.cl_int4, Pointer.to(maskSize_3D));
		clSetKernelArg(kernel_3D, arg++, Sizeof.cl_int4, Pointer.to(maskOrigins_3D));
		if ( color != 1 )
		{
			clSetKernelArg(kernel_3D, arg++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
	    clSetKernelArg(kernel_3D, arg, Sizeof.cl_int, Pointer.to(new int[]{0}));
	    int sliceArg_3D = arg;
	
	    long globalWorkSize[] = new long[]{width,height};        
	    
	    
		// create command queue:
		cl_command_queue commandQueue = 
				clCreateCommandQueue(cl, device, 0, null);
	    for ( int i = 0; i < depth; i++ )
	    {
	    	if ( (i < maskSize_3D[2]) || (i >= depth - maskSize_3D[2]) )
	    	{
	    		clSetKernelArg(kernel_2D, sliceArg_2D, Sizeof.cl_int, Pointer.to(new int[]{i}));
	    		clEnqueueNDRangeKernel(commandQueue, kernel_2D, 2, null, globalWorkSize, null, 0, null, null);
	    	}
	    	else
	    	{
	    		clSetKernelArg(kernel_3D, sliceArg_3D, Sizeof.cl_int, Pointer.to(new int[]{i}));
	    		clEnqueueNDRangeKernel(commandQueue, kernel_3D, 2, null, globalWorkSize, null, 0, null, null);
	    	}
		}
		clEnqueueReadBuffer(commandQueue, outputBuffer, CL_TRUE, 0, Sizeof.cl_float * output.length, Pointer.to(output), 0, null, null);
		saveImage(output, time, true );
		
		clReleaseMemObject(inputBuffer);
		clReleaseMemObject(outputBuffer);
	}

	private void gaussianBlurSep3D( int time )
	{
		initCL(m_iDeviceType, null);

		int nBuffers = 2;
		int elementCount = width * height * depth * color;		
		long maxAllocSize = OpenCLAlgorithmBase.getLong(device, CL_DEVICE_MAX_MEM_ALLOC_SIZE);
		if ( elementCount > (maxAllocSize / (nBuffers * Sizeof.cl_float)) )
		{
			System.err.println( "Calling gaussianBlurSep25DSlices" );
			gaussianBlurSep25DSlices();
			return;
		}
		float[] input = new float[ elementCount ];
		try {
			if ( this.entireImage )
			{
				srcImage.exportData( time * input.length, input.length, input );
			}
			else
			{
				srcImage.exportDataUseMask( time * input.length, input.length, input );
			}
		} catch (IOException e) {
			e.printStackTrace();
		}

		float[] output = new float[ elementCount ];

		int[] errcode = new int[1];
		cl_mem inputBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * input.length, Pointer.to(input), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		cl_mem outputBuffer = clCreateBuffer(cl, CL.CL_MEM_WRITE_ONLY,
				Sizeof.cl_float * output.length, null, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		// Convolve Seperable:
		GaussianKernelFactory gkf = GaussianKernelFactory.getInstance(sigmas);
		gkf.setKernelType(GaussianKernelFactory.BLUR_KERNEL);
		Kernel gaussianKernel = gkf.createKernel();
		OpenCLAlgorithmConvolver.convolveSep3D( cl, device, 
				inputBuffer, outputBuffer, width, height, depth, elementCount, gaussianKernel, 
				color, colorMask, false );

        
		// create command queue:
		cl_command_queue commandQueue = 
				clCreateCommandQueue(cl, device, 0, null);
    	clEnqueueReadBuffer(commandQueue, outputBuffer, CL_TRUE, 0, Sizeof.cl_float * output.length, Pointer.to(output), 0, null, null);
		saveImage(output, time, true );
		
		clReleaseMemObject(inputBuffer);
		clReleaseMemObject(outputBuffer);
	}
	
	/**
	 * Calls gaussianBlur3D for each volume in the time series image.
	 */
	private void gaussianBlur4D( )
	{
		for ( int i = 0; i < time; i++ )
		{
			if ( !separable )
			{
				gaussianBlur3D(i);
			}
			else
			{
				gaussianBlurSep3D(i);
			}
		}
	}
}

