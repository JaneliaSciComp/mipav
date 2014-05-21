package gov.nih.mipav.model.algorithms;



import static org.jocl.CL.*;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Vector;

import javax.media.nativewindow.NativeSurface;
import javax.media.opengl.GL;
import javax.media.opengl.GL3;
import javax.media.opengl.GL3bc;
import javax.media.opengl.GLContext;

import jogamp.opengl.GLContextImpl;
import jogamp.opengl.GLDrawableImpl;
import jogamp.opengl.egl.EGLContext;
import jogamp.opengl.macosx.cgl.MacOSXCGLContext;
import jogamp.opengl.windows.wgl.WindowsWGLContext;
import jogamp.opengl.x11.glx.X11GLXContext;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;

import org.jocl.*;


/**
 * Abstract base class for implementing Algorithms in OpenCL.
 * Extends AlgorithmBase.
 */
public abstract class OpenCLAlgorithmBase extends AlgorithmBase {

	protected static Vector<cl_context> contextLibrary = null;
	protected static Vector<Long> openCLTypeLibrary = null;
	protected cl_context cl = null;
	protected cl_device_id device = null;

	private static boolean isOCLAvailable = false;
	private static boolean isOCL_CPU_Available = false;
	private static boolean isOCL_GPU_Available = false;
	private static boolean wasOCLChecked = false;
	
    // The platform, device type and device number that will be used
    private static final int platformIndex = 0;
    private static final long deviceType = CL_DEVICE_TYPE_ALL;
    private static final int deviceIndex = 0;
    
    private static int m_iMajorVersion;
	private static int m_iMinorVersion;
    
	protected int width, height, depth, time, color;

    /** Color flags. */
	protected int[] colorMask = new int[]{0,0,0,0};

	protected long m_iDeviceType = CL.CL_DEVICE_TYPE_GPU;
	protected GL3 m_kGL = null;

	/**
	 * Flag, if true, indicates that the whole image should be processed. 
	 * If false only process the image over the mask areas.
	 */
	protected boolean entireImage;
	public OpenCLAlgorithmBase() {}

	public OpenCLAlgorithmBase(final ModelImage destImg, final ModelImage srcImg, final boolean entireImage, final long type)
	{

		super(destImg, srcImg);
		width  = srcImg.getExtents().length > 0 ? srcImg.getExtents()[0] : 1;
		height = srcImg.getExtents().length > 1 ? srcImg.getExtents()[1] : 1;
		depth  = srcImg.getExtents().length > 2 ? srcImg.getExtents()[2] : 1;
		time   = srcImg.getExtents().length > 3 ? srcImg.getExtents()[3] : 1;
		color = srcImage.isColorImage() ? 4 : 1;

		this.entireImage = entireImage;
		if (this.entireImage == false) {
			mask = srcImage.generateVOIMask();
		}
		m_iDeviceType = type;
	}


	protected void initCL( long iType, GL3 gl )
	{
		
		if ( contextLibrary != null )
		{
			for ( int i = 0; i < openCLTypeLibrary.size(); i++ )
			{
				if ( openCLTypeLibrary.elementAt(i) == iType )
				{
					cl = contextLibrary.elementAt(i);

					int[] refCount = new int[1];
					long[] actualCount = new long[1];
					clGetContextInfo( cl, CL.CL_CONTEXT_NUM_DEVICES, Sizeof.cl_int, Pointer.to(refCount), actualCount );
					cl_device_id[] deviceIDs = new cl_device_id[refCount[0]];
					clGetContextInfo( cl, CL.CL_CONTEXT_DEVICES, refCount[0]*Sizeof.cl_device_id, Pointer.to(deviceIDs), actualCount );
					device = deviceIDs[0];
					return;
				}
			}
		}
		
		
		cl_platform_id platform = null;

		int[] errcode = new int[1];
		int numPlatforms[] = new int[1];
		errcode[0] = clGetPlatformIDs(0, null, numPlatforms);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		//System.out.println("Number of platforms: "+numPlatforms[0]);
		cl_platform_id platforms[] = new cl_platform_id[numPlatforms[0]];
		errcode[0] = clGetPlatformIDs(platforms.length, platforms, null);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		// Collect all devices of all platforms
		for (int i=0; i<platforms.length; i++)
		{
			//String platformName = getString(platforms[i], CL_PLATFORM_NAME);

			// Obtain the number of devices for the current platform
			int numDevices[] = new int[1];
			try
			{
			if ( CL.CL_SUCCESS == clGetDeviceIDs(platforms[i], iType, 0, null, numDevices) )
			{
				//System.out.println("Number of devices in platform "+platformName+": "+numDevices[0]);
				if ( numDevices[0] > 0 )
				{
					cl_device_id devicesArray[] = new cl_device_id[numDevices[0]];
					//System.out.println ( stringFor_errorCode( clGetDeviceIDs(platforms[i], iType, numDevices[0], devicesArray, null) ) );
					errcode[0] = clGetDeviceIDs(platforms[i], iType, numDevices[0], devicesArray, null);
					if ( errcode[0] != CL.CL_SUCCESS )
					{
						System.err.println( stringFor_errorCode(errcode[0]) );
					}
					if ( devicesArray.length > 0)
					{
						device = devicesArray[0];
						platform = platforms[i];
					}
				}
			}
			} catch ( org.jocl.CLException e ) {}
		}

		if ( device == null )
		{
			System.err.println( "No such device..." );
			return;
		}

		
		// Initialize the context properties
		cl_context_properties contextProperties = new cl_context_properties();
		contextProperties.addProperty(CL_CONTEXT_PLATFORM, platform);
		if ( gl != null )
		{
			initContextProperties(contextProperties, gl);
		}
		cl = clCreateContext(
				contextProperties, 1, new cl_device_id[]{device}, 
				null, null, errcode);
//		long[] refCount = new long[1];
//		long[] actualCount = new long[1];
//		clGetContextInfo( cl, CL.CL_CONTEXT_REFERENCE_COUNT, Sizeof.cl_long, Pointer.to(refCount), actualCount );
//		System.err.println( "initCL " + cl.toString() + "    " + refCount[0] + "    " + actualCount[0] );
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}
		
		if ( contextLibrary == null )
		{
			contextLibrary = new Vector<cl_context>();
			openCLTypeLibrary = new Vector<Long>();
		}
		contextLibrary.add(cl);
		openCLTypeLibrary.add(iType);
		
		CL.setExceptionsEnabled(true);
	}

	/**
	 * Initializes the given context properties so that they may be
	 * used to create an OpenCL context for the given GL object.
	 *  
	 * @param contextProperties The context properties
	 * @param gl The GL object
	 */
	private static void initContextProperties(cl_context_properties contextProperties, GL gl)
	{
		// Adapted from [url]http://jogamp.org/jocl/www/[/url]

		GLContext glContext = gl.getContext();
		if(!glContext.isCurrent())
		{
			throw new IllegalArgumentException(
					"OpenGL context is not current. This method should be called " +
					"from the OpenGL rendering thread, when the context is current.");
		}

		long glContextHandle = glContext.getHandle();
		GLContextImpl glContextImpl = (GLContextImpl)glContext;
		GLDrawableImpl glDrawableImpl = glContextImpl.getDrawableImpl();
		NativeSurface nativeSurface = glDrawableImpl.getNativeSurface();

		if (glContext instanceof X11GLXContext)
		{
			long displayHandle = nativeSurface.getDisplayHandle();
			contextProperties.addProperty(CL_GL_CONTEXT_KHR, glContextHandle);
			contextProperties.addProperty(CL_GLX_DISPLAY_KHR, displayHandle);
		}
		else if (glContext instanceof WindowsWGLContext)
		{
			long surfaceHandle = nativeSurface.getSurfaceHandle();
			contextProperties.addProperty(CL_GL_CONTEXT_KHR, glContextHandle);
			contextProperties.addProperty(CL_WGL_HDC_KHR, surfaceHandle);
		}
		else if (glContext instanceof MacOSXCGLContext)
		{
			contextProperties.addProperty(CL_CGL_SHAREGROUP_KHR, glContextHandle);
		}
		else if (glContext instanceof EGLContext)
		{
			long displayHandle = nativeSurface.getDisplayHandle();
			contextProperties.addProperty(CL_GL_CONTEXT_KHR, glContextHandle);
			contextProperties.addProperty(CL_EGL_DISPLAY_KHR, displayHandle);
		}
		else
		{
			throw new RuntimeException("unsupported GLContext: " + glContext);
		}
	}	
	
	/**
	 * Checks the OpenCL error status.
	 * @param errcode
	 */
	protected void checkError( int errcode )
	{
		if ( errcode != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode) );
		}
	}


	/**
	 * Prepare this class for destruction.
	 */
	public void finalize() {
		destImage = null;
		srcImage = null;
		super.finalize();
	}

	public void saveImage(float[] data, int time, int zSlice, boolean calcMinMax )
	{
		if ( destImage == null )
		{
			try {
				srcImage.importData(zSlice * data.length, data, false);
			} catch (IOException e) {
				e.printStackTrace();
			}
			if ( calcMinMax )
			{
				srcImage.calcMinMax();
			}
		}
		else
		{
			try {
				destImage.importData(zSlice * data.length, data, false);
			} catch (IOException e) {
				e.printStackTrace();
			}
			if ( calcMinMax )
			{
				destImage.calcMinMax();
			}
		}
	}

	public void saveImage(float[] data, int time, boolean calcMinMax )
	{
		if ( destImage == null )
		{
			try {
				srcImage.importData(time * data.length, data, false);
			} catch (IOException e) {
				e.printStackTrace();
			}
			if ( calcMinMax )
			{
				srcImage.calcMinMax();
			}
		}
		else
		{
			try {
				if ( destImage.getNDims() == 3 )
				{
					destImage.importData(0, data, false);					
				}
				else
				{
					destImage.importData(time * data.length, data, false);
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
			if ( calcMinMax )
			{
				destImage.calcMinMax();
			}
		}
	}

	public static boolean isOCLAvailable()
	{
		if ( isOCLAvailable )
		{
			return true;
		}
		if ( wasOCLChecked )
		{
			return isOCLAvailable;
		}
		wasOCLChecked = true;

		int numPlatforms[] = new int[1];
		
		try 
		{
            clGetPlatformIDs(0, null, numPlatforms);
        } catch(UnsatisfiedLinkError ule) 
        {
            System.err.println("OpenCL libs unavailable, updated drivers needed to run OpenCL 1.2 code.");
            //ule.printStackTrace();
            return false;
        }
		    
		//System.out.println("Number of platforms: "+numPlatforms[0]);
		cl_platform_id platforms[] = new cl_platform_id[numPlatforms[0]];
		clGetPlatformIDs(platforms.length, platforms, null);
        
		cl_platform_id platform;
		try {
			platform = platforms[platformIndex];
		} catch (ArrayIndexOutOfBoundsException e) {
			System.err.println("OpenCL processing enabled, but no supported platforms are currently present.  This may occur if running MIPAV via Remote Desktop.");
			return false;
		}


        // Check if the platform supports OpenCL 1.2
        long sizeArray[] = { 0 };
        clGetPlatformInfo(platform, CL_PLATFORM_VERSION, 0, null, sizeArray);
        byte buffer[] = new byte[(int)sizeArray[0]];
        clGetPlatformInfo(platform, CL_PLATFORM_VERSION,
            buffer.length, Pointer.to(buffer), null);
        String versionString = new String(buffer, 0, buffer.length-1);
        //System.out.println("Platform version: "+versionString);
        String versionNumberString = versionString.substring(7, 10);
        try
        {
            String majorString = versionNumberString.substring(0, 1);
            String minorString = versionNumberString.substring(2, 3);
            int major = Integer.parseInt(majorString);
            int minor = Integer.parseInt(minorString);
            m_iMajorVersion = major;
            m_iMinorVersion = minor;
            if (major == 1 && minor < 2)
            {
               // System.err.println(
               //     "Platform only supports OpenCL "+versionNumberString);
               //System.exit(1);
            }
        }
        catch (NumberFormatException e)
        {
            System.err.println(
                "Invalid version number: "+versionNumberString);
            System.exit(1);
        }
		
		

		// Collect all devices of all platforms
		List<cl_device_id> devices = new ArrayList<cl_device_id>();
		for (int i=0; i<platforms.length; i++)
		{
			//String platformName = getString(platforms[i], CL_PLATFORM_NAME);

			// Obtain the number of devices for the current platform
			int numDevices[] = new int[1];
			clGetDeviceIDs(platforms[i], CL_DEVICE_TYPE_ALL, 0, null, numDevices);

			//System.out.println("Number of devices in platform "+platformName+": "+numDevices[0]);

			cl_device_id devicesArray[] = new cl_device_id[numDevices[0]];
			clGetDeviceIDs(platforms[i], CL_DEVICE_TYPE_ALL, numDevices[0], devicesArray, null);
			devices.addAll(Arrays.asList(devicesArray));
		}

		// Print the infos about all devices
		for (cl_device_id device : devices)
		{
			// CL_DEVICE_NAME
			//String deviceName = getString(device, CL_DEVICE_NAME);
			//System.out.println("--- Info for device "+deviceName+": ---");
			//System.out.printf("CL_DEVICE_NAME: \t\t\t%s\n", deviceName);

			// CL_DEVICE_VENDOR
			//String deviceVendor = getString(device, CL_DEVICE_VENDOR);
			//System.out.printf("CL_DEVICE_VENDOR: \t\t\t%s\n", deviceVendor);

			// CL_DRIVER_VERSION
			//String driverVersion = getString(device, CL_DRIVER_VERSION);
			//System.out.printf("CL_DRIVER_VERSION: \t\t\t%s\n", driverVersion);

			// CL_DEVICE_TYPE
			long deviceType = getLong(device, CL_DEVICE_TYPE);
			if( (deviceType & CL_DEVICE_TYPE_CPU) != 0)
			{
				//System.out.printf("CL_DEVICE_TYPE:\t\t\t\t%s\n", "CL_DEVICE_TYPE_CPU");
				isOCL_CPU_Available = true;
			}
			if( (deviceType & CL_DEVICE_TYPE_GPU) != 0)
			{
				//System.out.printf("CL_DEVICE_TYPE:\t\t\t\t%s\n", "CL_DEVICE_TYPE_GPU");
				isOCL_GPU_Available = true;
			}
		}
		isOCLAvailable = isOCL_CPU_Available | isOCL_GPU_Available;
		return isOCLAvailable;
	}	


	protected static long roundUp(long groupSize, long globalSize) {
		long r = globalSize % groupSize;
		if (r == 0) return globalSize;
		else        return globalSize + groupSize - r;
	}


	/**
	 * Returns the value of the device info parameter with the given name
	 *
	 * @param device The device
	 * @param paramName The parameter name
	 * @return The value
	 */
	public static int getInt(cl_device_id device, int paramName)
	{
		return getInts(device, paramName, 1)[0];
	}

	/**
	 * Returns the values of the device info parameter with the given name
	 *
	 * @param device The device
	 * @param paramName The parameter name
	 * @param numValues The number of values
	 * @return The value
	 */
	public static int[] getInts(cl_device_id device, int paramName, int numValues)
	{
		int values[] = new int[numValues];
		clGetDeviceInfo(device, paramName, Sizeof.cl_int * numValues, Pointer.to(values), null);
		return values;
	}

	/**
	 * Returns the value of the device info parameter with the given name
	 *
	 * @param device The device
	 * @param paramName The parameter name
	 * @return The value
	 */
	public static long getLong(cl_device_id device, int paramName)
	{
		return getLongs(device, paramName, 1)[0];
	}

	/**
	 * Returns the values of the device info parameter with the given name
	 *
	 * @param device The device
	 * @param paramName The parameter name
	 * @param numValues The number of values
	 * @return The value
	 */
	public static long[] getLongs(cl_device_id device, int paramName, int numValues)
	{
		long values[] = new long[numValues];
		clGetDeviceInfo(device, paramName, Sizeof.cl_long * numValues, Pointer.to(values), null);
		return values;
	}

	/**
	 * Returns the value of the device info parameter with the given name
	 *
	 * @param device The device
	 * @param paramName The parameter name
	 * @return The value
	 */
	public static String getString(cl_device_id device, int paramName)
	{
		// Obtain the length of the string that will be queried
		long size[] = new long[1];
		clGetDeviceInfo(device, paramName, 0, null, size);

		// Create a buffer of the appropriate size and fill it with the info
		byte buffer[] = new byte[(int)size[0]];
		clGetDeviceInfo(device, paramName, buffer.length, Pointer.to(buffer), null);

		// Create a string from the buffer (excluding the trailing \0 byte)
		return new String(buffer, 0, buffer.length-1);
	}

	/**
	 * Returns the value of the platform info parameter with the given name
	 *
	 * @param platform The platform
	 * @param paramName The parameter name
	 * @return The value
	 */
	public static String getString(cl_platform_id platform, int paramName)
	{
		// Obtain the length of the string that will be queried
		long size[] = new long[1];
		clGetPlatformInfo(platform, paramName, 0, null, size);

		// Create a buffer of the appropriate size and fill it with the info
		byte buffer[] = new byte[(int)size[0]];
		clGetPlatformInfo(platform, paramName, buffer.length, Pointer.to(buffer), null);

		// Create a string from the buffer (excluding the trailing \0 byte)
		return new String(buffer, 0, buffer.length-1);
	}

    /**
     * Sets the flag for the blue channel.
     *
     * @param  flag  if set to true then the blue channel is processed.
     */
    public void setBlue(boolean flag) {
        colorMask[3] = flag ? 1 : 0;
    }

    /**
     * Sets the flag for the green channel.
     *
     * @param  flag  if set to true then the green channel is processed.
     */
    public void setGreen(boolean flag) {
        colorMask[2] = flag ? 1 : 0;
    }

    /**
     * Sets the flag for the red channel.
     *
     * @param  flag  if set to true then the red channel is processed.
     */
    public void setRed(boolean flag) {
        colorMask[1] = flag ? 1 : 0;
    }
    
    public void setTime( int timeSlice )
    {
    	time = timeSlice;
    }

	/**
	 * Helper function which reads the file with the given name and returns 
	 * the contents of this file as a String. Will exit the application
	 * if the file can not be read.
	 * 
	 * @param fileName The name of the file to read.
	 * @return The contents of the file
	 */
	public static String readKernelFile(String fileName)
	{	
		String line = null;
        BufferedReader input = null;

        try {
            // use this long call instead of ClassLoader.getSystemResource() to work properly from a jnlp launch
            final URL fileURL = Thread.currentThread().getContextClassLoader().getResource("kernels/" + fileName);
            
            if (fileURL == null) {
                Preferences.debug("Unable to open " + fileName
                        + ".  Make sure it is in the same directory as MipavMain.class\n", Preferences.DEBUG_MINOR);

                return null;
            }
            
            //System.err.println( fileURL );
            //String fileLocation = fileURL.getPath() + File.separator + fileName;
            //FileInputStream file = new FileInputStream( fileLocation );

            // use buffering
            // this implementation reads one line at a time
            input = new BufferedReader(new InputStreamReader(fileURL.openStream()));

			StringBuffer sb = new StringBuffer();
			while (true)
			{
				line = input.readLine();
				if (line == null)
				{
					break;
				}
				sb.append(line).append("\n");
			}
			input.close();
			return sb.toString();
        } catch (final Exception ex) {} finally {

            try {

                if (input != null) {
                    input.close();
                    input = null;
                }
            } catch (final IOException closee) {}
        }
		return null;
	}


	/**
	 * Determines the amount of memory used for the OpenCL buffers
	 * required for the gaussian kernels for the given sigmas.
	 * Plus the memory used for the nBuffers OpenCL buffers, each
	 * elementCount in size.
	 * @param nBuffers number of buffers
	 * @param elementCount buffer size
	 * @param sigmas
	 * @return
	 */
	protected long getMaxMemoryUsed( int nBuffers, int elementCount, float[] sigmas )
	{
		int kernelSize = 0;
		for ( int i = 0; i < sigmas.length; i++ )
		{
			int halfWidth = (int)(4.0 * sigmas[i] + 0.5);
			int size = (2 * halfWidth + 1);
			kernelSize += size;
		}
		return kernelSize + nBuffers * elementCount;
	}
	
}
