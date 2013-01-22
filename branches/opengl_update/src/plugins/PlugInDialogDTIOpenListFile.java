import java.awt.Cursor;
import java.awt.event.ActionEvent;
import java.io.File;

import javax.swing.JFileChooser;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;

import gov.nih.mipav.view.dialogs.JDialogBase;

/**
 * This is the main dialog for the DTI Open List File Plug-In
 * This plugin opens a 4D DTI dataset by reading in the list file
 * @author pandyan
 * 
 * 
 * References: This algorithm was developed in concert with Lin-Ching Chang D.Sc., Carlo Pierpaoli MD Ph.D., and Lindsay Walker MS of
 * the NIH/NICHD/LIMB/STBB group :
 * 
 * Section on Tissue Biophysics and Biomimetics (STBB)
 * Laboratory of Integrative and Medical Biophysics (LIMB)
 * National Institute of Child Health & Humann Development
 * National Institutes of Health
 *
 */
public class PlugInDialogDTIOpenListFile extends JDialogBase implements AlgorithmInterface {

	/** path to list file **/
	private String absPath;
	
	/** parent dir **/
	private String parentDir;
	
	/** handle to algorithm **/
	private PlugInAlgorithmDTIOpenListFile alg;
	
	private ModelImage resultImage;

	
	
	
	/**
	 * Constructor
	 */
	public PlugInDialogDTIOpenListFile(boolean modal) {
		super(modal);


		JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
        chooser.setDialogTitle("Choose list file");
        int returnValue = chooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) {
        	if(!(chooser.getSelectedFile().getName().endsWith(".list"))) {
        		MipavUtil.displayError("File selected must have a .list extension");
        		dispose();
        	}else {
        		absPath = chooser.getSelectedFile().getAbsolutePath();
        		parentDir = chooser.getSelectedFile().getParent();
        		callAlgorithm();
        	}
        }else {
        	dispose();
        }
	}
	
	/**
	 * call algorithm
	 *
	 */
	public void callAlgorithm() {
		
		alg = new PlugInAlgorithmDTIOpenListFile(absPath,parentDir);
		
		alg.addListener(this);
		
		createProgressBar("Open list file", alg);
		
		if (isRunInSeparateThread()) {
			// Start the thread as a low priority because we wish to still
			// have user interface work fast.
			if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
				MipavUtil.displayError("A thread is already running on this object");
			}
		} else {
			alg.run();
		}
		
	}
	
	/**
	 * algorithm performed
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		if(alg.isCompleted()) {

			resultImage = alg.getDestImage();
			
			try {
                new ViewJFrameImage(resultImage, (ModelLUT)null);
            } catch (Exception error) {
            	error.printStackTrace();
                MipavUtil.displayError("Out of memory: unable to open new frame");
            }
            setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
			alg = null;
			
		}

	}

}
