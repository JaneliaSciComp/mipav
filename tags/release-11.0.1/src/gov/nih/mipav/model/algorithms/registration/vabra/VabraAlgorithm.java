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
	protected ModelImage registeredResults;
	
	public VabraAlgorithm() {}

	/**
	 * @param subject The 3D B0 Image that will be registered to the target.
	 * @param target  The 3D Structural image.
	 * @return the list of ModelImages that are the registered subjects.
	 */
	public ModelImage solve(ModelImage subject, ModelImage target) {
		/*
		registeredResults = new ArrayList<ModelImage>();
		registeredResults.add( VabraSubjectTargetPairs.convertToModelImage( (ImageDataFloat)VabraSubjectTargetPairs.convertToImage( subject ) ) );
		deformationField = VabraSubjectTargetPairs.convertToModelImage( (ImageDataFloat)VabraSubjectTargetPairs.convertToImage( target ) );
		*/
		long startTime = System.currentTimeMillis();
		
		subject.calcMinMax();

		target.calcMinMax();
		
		
		int InterpType = RegistrationUtilities.InterpolationType.TRILINEAR;
		double[] directionsOptmizationWeight = {1, 1, 1};

        final URL fileURL = Thread.currentThread().getContextClassLoader().getResource("config.xml");
        
        if (fileURL == null) {
            Preferences.debug("Unable to open " + "config.xml"
                    + ".  Make sure it is in the same directory as MipavMain.class\n", Preferences.DEBUG_MINOR);

        }
        File config = null;
        
        if (fileURL != null) {
			try {
				config = new File(fileURL.toURI());
			} catch (URISyntaxException e) {
				e.printStackTrace();
			}
        }
        
		float robustMaxT = 0.000f;
		float robustMinT = 0.000f;
		int numBins = 64;
		int defFieldUpdateMode = 0;
		boolean useMNMI = false;
		
		//System.out.println(getClass().getCanonicalName()+"\t"+"VABRA-ALG: Before RBFPair");

		
		//1.)Construct Target and Subject Pairs 
		VabraSubjectTargetPairs imgSubTarPairs = new VabraSubjectTargetPairs(subject, target, 
				robustMaxT, robustMinT, numBins, InterpType, useMNMI, directionsOptmizationWeight, defFieldUpdateMode);
		
		//2.)Construct Vabra Solver
		//VabraSolver solver = new VabraSolver(imgSubTarPairs, config, new File(subject.getImageDirectory()), true, directionsOptmizationWeight, defFieldUpdateMode);
		VabraSolver solver = new VabraSolver(imgSubTarPairs, config, null, false);

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
		
		long now = System.currentTimeMillis();
		double elapsedTime = (double) (now - startTime);

		// if elasedTime is invalid, then set it to 0
		if (elapsedTime <= 0) {
			elapsedTime = (double) 0.0;
		}
		double timeinSec =  (double) (elapsedTime / 1000.0);        
		System.err.println( "Elapsed time 4D reg: " + timeinSec );

		return registeredResults;
	}
	
	public ModelImage getDeformationField() {
		return deformationField;
	}
	
	public ModelImage getRegisteredResults() {
		return registeredResults;
	}
}
