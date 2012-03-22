package gov.nih.mipav.model.algorithms.registration.vabra;

import gov.nih.mipav.model.algorithms.utilities.AlgorithmSubset;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.io.File;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;


public class VabraAlgorithm  {

	protected ModelImage deformationField;
	protected List<ModelImage> registeredResults;
	
	public VabraAlgorithm() {}

	/**
	 * @param target4D  The 4D Diffusion-Weighted image series.
	 * @param subjects3D The 3D structural image (when null the B0 image is extracted and used as the subject)
	 * @return the list of ModelImages that are the registered subjects.
	 */
	public List<ModelImage> solve(ModelImage target4D, ModelImage subjects3D) {
		boolean bExtractB0 = true;
		List<ModelImage> subjectVols = new ArrayList<ModelImage>();
		if ( subjects3D != null )
		{
			subjectVols.add(subjects3D);
			bExtractB0 = false;
		}

		List<ModelImage> targetVols = new ArrayList<ModelImage>();
		int tDim = target4D.getExtents().length > 3 ? target4D.getExtents()[3] : 1;
		targetVols.add(target4D);
		
		if ( bExtractB0 )
		{
            float[] bvalues = target4D.getDTIParameters().getbValues();
			int[] extents = new int[]{ target4D.getExtents()[0], target4D.getExtents()[1], target4D.getExtents()[2] };
			for ( int i = 0; i < tDim; i++ )
			{
                if ( bvalues[i] == 0 )
                {
                	ModelImage resultImage = new ModelImage(target4D.getType(), extents, target4D.getImageName() + i);
                	AlgorithmSubset subsetAlgo = new AlgorithmSubset(target4D, resultImage, AlgorithmSubset.REMOVE_T, i );
                	subsetAlgo.run();
                	JDialogBase.updateFileInfo( target4D, resultImage );
                	subjectVols.add(resultImage);
                	resultImage.setImageName( target4D.getImageName() + "B0" );
                }
			}
		}
		
		int[] InterpType = new int[subjectVols.size()]; 
		for ( int i = 0; i < InterpType.length; i++ )
		{
			InterpType[i] = RegistrationUtilities.InterpolationType.TRILINEAR;
		}
		double[] directionsOptmizationWeight = {1, 1, 1};

        final URL fileURL = Thread.currentThread().getContextClassLoader().getResource("config.xml");
        
        if (fileURL == null) {
            Preferences.debug("Unable to open " + "config.xml"
                    + ".  Make sure it is in the same directory as MipavMain.class\n", Preferences.DEBUG_MINOR);

            return null;
        }
        File config = null;
		try {
			config = new File(fileURL.toURI());
		} catch (URISyntaxException e) {
			e.printStackTrace();
		}		
		float robustMaxT = 0.000f;
		float robustMinT = 0.000f;
		int numBins = VabraHistograms.defaultBins;
		int defFieldUpdateMode = 0;
		boolean useMNMI = false;
		
		//System.out.println(getClass().getCanonicalName()+"\t"+"VABRA-ALG: Before RBFPair");

		
		//1.)Construct Target and Subject Pairs 
		VabraSubjectTargetPairs imgSubTarPairs = new VabraSubjectTargetPairs(subjectVols, targetVols, 
				robustMaxT, robustMinT, numBins, InterpType, useMNMI);
		
		//2.)Construct Vabra Solver
		VabraSolver solver = new VabraSolver(imgSubTarPairs, config, null, false, directionsOptmizationWeight, defFieldUpdateMode);

		//System.out.println(getClass().getCanonicalName()+"\t"+"VABRA-ALG: Before Register");

		//3.)Register Images
		solver.registerImages();
		
		//4.)Set outputs
		registeredResults = solver.getDeformedSubject();
		deformationField = solver.getDeformationField();
		
		//System.out.println(getClass().getCanonicalName()+"\t"+"VABRA-ALG: Before Cleanup");
		

		if ( bExtractB0 )
		{
			for ( int i = subjectVols.size() -1; i >= 0; i-- )
			{
				ModelImage temp = subjectVols.remove(i);
				temp.disposeLocal();
				temp = null;
			}
		}

		imgSubTarPairs.dispose();
		solver.dispose();
		System.gc();

		return registeredResults;
	}
	
	public ModelImage getDeformationField() {
		return deformationField;
	}
	
	public List<ModelImage> getRegisteredResults() {
		return registeredResults;
	}
}
