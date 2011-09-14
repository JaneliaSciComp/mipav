package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.AlgorithmBase;

import com.jogamp.opencl.CLDevice;
import com.jogamp.opencl.CLPlatform;


public abstract class OpenCLAlgorithmBase extends AlgorithmBase {
    private static boolean isOCLAvailable = false;
    private static boolean isOCLAvailableCheck = false;
    public static boolean isOCLAvailable()
    {
        if ( isOCLAvailableCheck )
        {
            return isOCLAvailable;
        }
        CLDevice testGPU = null;
        try {
            CLPlatform[] platforms = CLPlatform.listCLPlatforms();
            for (CLPlatform platform : platforms) {
                testGPU = platform.getMaxFlopsDevice(CLDevice.Type.GPU);
                if(testGPU != null) {
                    isOCLAvailable = true;
                    break;
                }
            }
        }
        catch ( com.jogamp.opencl.CLException e1 ) {}
        catch ( com.jogamp.common.JogampRuntimeException e2 ) {}
        isOCLAvailableCheck = true;
        return isOCLAvailable;
    }
	
}
