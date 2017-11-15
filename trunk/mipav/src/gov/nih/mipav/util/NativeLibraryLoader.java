package gov.nih.mipav.util;


import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;

import java.io.File;

import org.scijava.nativelib.NativeLibraryUtil;
import org.scijava.nativelib.NativeLibraryUtil.Architecture;


/**
 * Static methods for extracting and loading native libraries that were previously loaded directly in the MIPAV JRE.
 * JHDF5, JOCL, and J3D libraries are the ones currently included.
 *
 */
public class NativeLibraryLoader {
    // load native libraries that previously were in the JRE
    private static final String[] libList = new String[] {
            "jhdf5",
            "JOCL-0.1.7",
            "j3dcore-d3d", 
            "j3dcore-ogl", 
            "j3dcore-ogl-cg",
            "j3dcore-ogl-chk",
    };
    
    /**
     * Loads the MIPAV native libraries after extracting them.
     * 
     * @return False if there was a problem extracting/loading the libraries.
     */
    public static boolean loadNativeLibraries() {
        final String tempDir = getNativeLibTmpDir();
        System.setProperty("java.library.tmpdir", tempDir);
        
        // create temp directory if it doesn't exist
        final File tmpDirFile = new File(tempDir);
        if (!tmpDirFile.exists()) {
            tmpDirFile.mkdirs();
            tmpDirFile.deleteOnExit();
        }
        
        // make sure that the temp dir creation succeeded
        if (!tmpDirFile.exists() || !tmpDirFile.canWrite()) {
            System.err.println("Native library error: cannot write to temporary directory: " + tempDir);
            return false;
        } else {
            //NativeLibraryLoader.createLockFile();
            
            for (final String curLib : libList) {
                NativeLibraryUtil.loadNativeLibrary(NativeLibraryUtil.class, curLib);
            }
            
            for (final File libFile : tmpDirFile.listFiles()) {
                libFile.deleteOnExit();
            }
        }
        
        return true;
    }
    
    public static String getNativeLibTmpDir() {
        String tempDir = Preferences.getPreferencesDir() + File.separator + "native_library_tmpdir" + File.separator + MipavUtil.getVersion();
        
        final String arch = getArchString();
        if (!arch.equals("")) {
            tempDir += "_" + arch;
        }
        
        
        return tempDir;
    }
    
    public static String getArchString() {
        Architecture arch = NativeLibraryUtil.getArchitecture();

        String archString = "";
        switch (arch) {
            case LINUX_32:
            case WINDOWS_32:
            case OSX_32:
                archString = "32";
                break;
            case LINUX_64:
            case WINDOWS_64:
            case OSX_64:
                archString = "64";
                break;
            case LINUX_ARM:
            case LINUX_ARM64:
                archString = "arm";
                break;
            case OSX_PPC:
                archString = "ppc";
            case UNKNOWN:
            default:
                archString = "";
        }

        return archString;
    }
}
