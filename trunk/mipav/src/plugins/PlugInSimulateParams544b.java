//MIPAV is freely available from http://mipav.cit.nih.gov

//THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, 
//EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES 
//OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND 
//NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT 
//HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
//WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
//FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE 
//OR OTHER DEALINGS IN THE SOFTWARE. 

/*****************************************************************
******************************************************************

The MIPAV application is intended for research use only.
This application has NOT been approved for ANY diagnostic use 
by the Food and Drug Administration. There is currently no 
approval process pending. 

This software may NOT be used for diagnostic purposes.

******************************************************************
******************************************************************/

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface

import gov.nih.mipav.view.*;

import java.awt.*;

import niaid.tumorSim.simParams.PlugInDialogSimulateParams544b;

/**
 * 
 * This is the main driver for a plug-in.  As a PlugInAlgorithm, it requires an image 
 * to run properly.  Its menu structure can be optionally specified as shown by CATEGORY.
 * By only placing this class in the mipav/plugins directory, one can modify the dialog and
 * algorithm class for plug-in development.  Those classes should remain in a user's workspace
 * until they have reached a stable state.
 * 
 * @author Justin Senseney (SenseneyJ@mail.nih.gov)
 * @see http://mipav.cit.nih.gov
 */

public class PlugInSimulateParams544b implements PlugInGeneric {

    public static final String[] CATEGORY = {"Parametric Maps", "Simulation", "544b"};

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Defines body of run method, which was declared in the interface. 
     *
     * @param  parentFrame  parent frame
     * @param  image        current ModelImage - this is an image already loaded into MIPAV. Can be null.
     *
     * @see    ModelImage
     * @see    ViewJFrameImage
     */
    public void run() {
        new PlugInDialogSimulateParams544b(false);
        
    }
}
