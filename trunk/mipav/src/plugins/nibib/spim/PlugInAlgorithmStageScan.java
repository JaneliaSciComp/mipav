package nibib.spim;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.ViewUserInterface;



public class PlugInAlgorithmStageScan extends AlgorithmBase {
	
	private final Collection<ModelImage> resultImageList;
	
	private final String AFileDirectory;
	
	public PlugInAlgorithmStageScan(final String AFileDirectory) {
		this.AFileDirectory = AFileDirectory;
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