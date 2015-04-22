package nibib.spim;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.ViewUserInterface;



public class PlugInAlgorithmStageScan extends AlgorithmBase {
	
	private final Collection<ModelImage> resultImageList;
	
	private final String AFileDirectory;
	
	private final String AFileDark2D;
	
	private final double ALeftShift;
	
	private File[] AImageAr;
	
    private final String BFileDirectory;
	
	private final String BFileDark2D;
	
	private final double BLeftShift;
	
	private File[] BImageAr;
	
	private File resultDirectory;
	
	public PlugInAlgorithmStageScan(final String AFileDirectory, final String AFileDark2D, final double ALeftShift,
			File[] AImageAr, final String BFileDirectory, final String BFileDark2D, final double BLeftShift,
			File[] BImageAr, File resultDirectory) {
		this.AFileDirectory = AFileDirectory;
		this.AFileDark2D = AFileDark2D;
		this.ALeftShift = ALeftShift;
		this.AImageAr = AImageAr;
		this.BFileDirectory = BFileDirectory;
		this.BFileDark2D = BFileDark2D;
		this.BLeftShift = BLeftShift;
		this.BImageAr = BImageAr;
		this.resultDirectory = resultDirectory;
		this.resultImageList = Collections.synchronizedCollection(new ArrayList<ModelImage>());
		
	}
	
	 // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    @Override
    public void finalize() {
        destImage = null;
        srcImage = null;
        super.finalize();
    }

    /**
     * Starts the algorithm. At the conclusion of this method, AlgorithmBase reports to any algorithm listeners that
     * this algorithm has completed. This method is not usually called explicitly by a controlling dialog. Instead, see
     * AlgorithmBase.run() or start().
     */
    @Override
    public void runAlgorithm() {

        final boolean appFrameFlag = ViewUserInterface.getReference().isAppFrameVisible();
        ViewUserInterface.getReference().setAppFrameVisible(false);
    }
    
    public Collection<ModelImage> getResultImageList() {
        return resultImageList;
    }
}