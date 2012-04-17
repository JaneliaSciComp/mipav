package gov.nih.mipav.model.algorithms.registration.vabra;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.Preferences;

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
	 * @param subject The 3D B0 Image that will be registered to the target.
	 * @param target  The 3D Structural image.
	 * @return the list of ModelImages that are the registered subjects.
	 */
	public List<ModelImage> solve(ModelImage subject, ModelImage target) {
		/*
		registeredResults = new ArrayList<ModelImage>();
		registeredResults.add( VabraSubjectTargetPairs.convertToModelImage( (ImageDataFloat)VabraSubjectTargetPairs.convertToImage( subject ) ) );
		deformationField = VabraSubjectTargetPairs.convertToModelImage( (ImageDataFloat)VabraSubjectTargetPairs.convertToImage( target ) );
		*/
		
		
		List<ModelImage> subjectVols = new ArrayList<ModelImage>();
		subjectVols.add(subject);

		List<ModelImage> targetVols = new ArrayList<ModelImage>();
		targetVols.add(target);
		
		
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
		//VabraSolver solver = new VabraSolver(imgSubTarPairs, config, new File(subject.getImageDirectory()), true, directionsOptmizationWeight, defFieldUpdateMode);
		VabraSolver solver = new VabraSolver(imgSubTarPairs, config, null, false, directionsOptmizationWeight, defFieldUpdateMode);

		//System.out.println(getClass().getCanonicalName()+"\t"+"VABRA-ALG: Before Register");

		//3.)Register Images
		solver.registerImages();
		
		//4.)Set outputs
		registeredResults = solver.getDeformedSubject();
		deformationField = solver.getDeformationField();
		
		//System.out.println(getClass().getCanonicalName()+"\t"+"VABRA-ALG: Before Cleanup");
		
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
