package gov.nih.mipav.view.renderer.WildMagic.Interface;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.FocusEvent;
import java.awt.event.WindowEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Matrix4f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Mathematics.Vector4f;

import gov.nih.mipav.model.algorithms.AlgorithmArcLength;
import gov.nih.mipav.model.algorithms.AlgorithmBSmooth;
import gov.nih.mipav.model.algorithms.AlgorithmBSpline;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.provenance.ProvenanceRecorder;
import gov.nih.mipav.model.scripting.ScriptRecorder;
import gov.nih.mipav.model.scripting.actions.ActionSaveAllVOIs;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIPoint;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.ViewVOIVector;
import gov.nih.mipav.view.components.PanelManager;
import gov.nih.mipav.view.dialogs.GuiBuilder;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;

public class JDialogLattice extends JDialogBase {

	private ModelImage image;    
	private JTextField[][] pairFields;
	private JPanel okCancelPanel;    
	private VOI lattice = null;
	private int numPoints = 0;
	private int numPairs = 0;
	private Vector3f[] left_right_markers;
	private JButton saveButton;
	
	public JDialogLattice( ModelImage image )
	{
		this.image = image;
		init();
		setVisible(true);
	}

    public void actionPerformed(ActionEvent event) {
		String command = event.getActionCommand();
		Object source = event.getSource();

		if (command.equals("OK")) {
			setVariables();
			this.windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
		} else if (command.equals("Cancel")) {
			this.windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
		} else if ( source == saveButton ) {
			saveLattice();
		} else {
			setVariables();
		}
    }
    
    private void saveLattice()
    {
        final JFileChooser chooser = new JFileChooser();

        if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
        } else {
            chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
        }

        chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        final int returnVal = chooser.showSaveDialog(null);

        String fileName = null, directory = null, voiDir;
        if (returnVal == JFileChooser.APPROVE_OPTION) {
            fileName = chooser.getSelectedFile().getName();
            directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            Preferences.setProperty(Preferences.PREF_VOI_LPS_SAVE, "true");
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
        }

        if (fileName != null) {
            voiDir = new String(directory + fileName + File.separator);                          
            
            VOIVector backUpVOIs = image.getVOIsCopy();

			image.resetVOIs();
			image.registerVOI(lattice);
			lattice.setColor( new Color( 0, 0, 255) );
			lattice.getCurves().elementAt(0).update( new ColorRGBA(0,0,1,1));
			lattice.getCurves().elementAt(1).update( new ColorRGBA(0,0,1,1));
			lattice.getCurves().elementAt(0).setClosed(false);
			lattice.getCurves().elementAt(1).setClosed(false);
			for ( int j = 0; j < lattice.getCurves().elementAt(0).size(); j++ )
			{
				short id = (short) image.getVOIs().getUniqueID();
				VOI marker = new VOI(id, "pair_" + j, VOI.POLYLINE, (float)Math.random() );
				VOIContour mainAxis = new VOIContour(false); 		    		    		
				mainAxis.add( lattice.getCurves().elementAt(0).elementAt(j) );
				mainAxis.add( lattice.getCurves().elementAt(1).elementAt(j) );
				marker.getCurves().add(mainAxis);
				marker.setColor( new Color( 255, 255, 0) );
				mainAxis.update( new ColorRGBA(1,1,0,1));
				if ( j == 0 )
				{
					marker.setColor( new Color( 0, 255, 0) );
					mainAxis.update( new ColorRGBA(0,1,0,1));
				}
				image.registerVOI( marker );
			}
			
			saveAllVOIsTo( voiDir, image );    
            
			image.restoreVOIs(backUpVOIs);
        }

    }
    
    private void saveAllVOIsTo(final String voiDir, ModelImage image) {
        try {
            ViewVOIVector VOIs = image.getVOIs();

            final File voiFileDir = new File(voiDir);

            if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
            } else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
            } else { // voiFileDir does not exist
                voiFileDir.mkdir();
            }

            int nVOI = VOIs.size();

            for (int i = 0; i < nVOI; i++) {
                if (VOIs.VOIAt(i).getCurveType() != VOI.ANNOTATION) {
                	FileVOI fileVOI = new FileVOI(VOIs.VOIAt(i).getName() + ".xml", voiDir, image);
                	fileVOI.writeXML(VOIs.VOIAt(i), true, true);
                }
                else {
                    FileVOI fileVOI = new FileVOI(VOIs.VOIAt(i).getName() + ".lbl", voiDir, image);
                    fileVOI.writeAnnotationInVoiAsXML(VOIs.VOIAt(i).getName(),true);             	
                }
            }

        } catch (final IOException error) {
            MipavUtil.displayError("Error writing all VOIs to " + voiDir + ": " + error);
        }

    } // end saveAllVOIsTo()
    
    
    private void setVariables()
    {
    	int count = 0;
    	int[][] pairs = new int[numPairs][2];
    	for ( int i = 0; i < numPairs; i++ )
    	{
    		pairs[i][0] = -1;
    		pairs[i][1] = -1;
    		if ( (pairFields[i][0].getText().length() > 0) && (pairFields[i][1].getText().length() > 0) )
    		{
    			if ( testParameter( pairFields[i][0].getText(), 0, numPoints ) );
    			{
    				pairs[i][0] = Integer.valueOf(pairFields[i][0].getText()).intValue();
    				count++;
    			}
    			if ( testParameter( pairFields[i][1].getText(), 0, numPoints ) );
    			{
    				pairs[i][1] = Integer.valueOf(pairFields[i][1].getText()).intValue();
    				count++;
    			}
    		}
    	}    	
    	if ( count > 0 )
    	{
    		VOIVector VOIs = image.getVOIs();
//    		VOIVector VOIs = image.getVOIsCopy();
//    		for ( int i = VOIs.size()-1; i >= 0; i-- )
//    		{
//    			VOI currentVOI = VOIs.elementAt(i);
//    			if ( currentVOI.getCurveType() == VOI.POLYLINE )
//    			{
//    				VOIs.remove(currentVOI);
//    			}
//    		}
    		
    		
    		if ( lattice == null )
    		{
    			short id = (short) image.getVOIs().getUniqueID();
    			lattice = new VOI(id, "lattice", VOI.POLYLINE, (float)Math.random() );
    			VOIContour leftSide = new VOIContour( false );
    			VOIContour rightSide = new VOIContour( false );
    			lattice.getCurves().add(leftSide);		
    			lattice.getCurves().add(rightSide);
    		}
    		VOIContour leftSide = (VOIContour) lattice.getCurves().elementAt(0);
    		VOIContour rightSide = (VOIContour) lattice.getCurves().elementAt(1);
    		leftSide.clear();
    		rightSide.clear();
    		for ( int i = 0; i < numPairs; i++ )
    		{
    			if ( (pairs[i][0] == -1) || (pairs[i][1] == -1) )
    			{
    				break;
    			}
    			
    			leftSide.add( left_right_markers[ pairs[i][0] ] );
    			rightSide.add( left_right_markers[ pairs[i][1] ] );
    		}
    		
    		if ( leftSide.size() > 1 )
    		{
    			image.registerVOI(lattice);
    		}
			lattice.setColor( new Color( 0, 0, 255) );
			lattice.getCurves().elementAt(0).update( new ColorRGBA(0,0,1,1));
			lattice.getCurves().elementAt(1).update( new ColorRGBA(0,0,1,1));
			lattice.getCurves().elementAt(0).setClosed(false);
			lattice.getCurves().elementAt(1).setClosed(false);
			for ( int j = 0; j < leftSide.size(); j++ )
			{
				short id = (short) image.getVOIs().getUniqueID();
				VOI marker = new VOI(id, "pair_" + j, VOI.POLYLINE, (float)Math.random() );
				VOIContour mainAxis = new VOIContour(false); 		    		    		
				mainAxis.add( leftSide.elementAt(j) );
				mainAxis.add( rightSide.elementAt(j) );
				marker.getCurves().add(mainAxis);
				marker.setColor( new Color( 255, 255, 0) );
				mainAxis.update( new ColorRGBA(1,1,0,1));
				if ( j == 0 )
				{
					marker.setColor( new Color( 0, 255, 0) );
					mainAxis.update( new ColorRGBA(0,1,0,1));
				}
				image.registerVOI( marker );
			}
//			parent.restoreVOIs(VOIs);
    	}
    }
	
	private void init()
	{
		setResizable(true);
		setForeground(Color.black);
		setTitle("Build 3D Lattice");
		try {
			setIconImage(MipavUtil.getIconImage("divinci.gif"));
		} catch (FileNotFoundException e) {
			Preferences.debug("Failed to load default icon", Preferences.DEBUG_MINOR);
		}

		GuiBuilder gui = new GuiBuilder(this);
		
		numPoints = 0;
		VOIVector VOIs = image.getVOIs();
		for ( int i = 0; i < VOIs.size(); i++ )
		{
			VOI currentVOI = VOIs.elementAt(i);
			if ( currentVOI.getCurveType() == VOI.ANNOTATION )
			{
				numPoints++;
			}
		}
		if ( numPoints == 0 )
		{
			return;
		}
		left_right_markers = new Vector3f[numPoints];
		for ( int i = 0; i < VOIs.size(); i++ )
		{
			VOI currentVOI = VOIs.elementAt(i);
			if ( currentVOI.getCurveType() == VOI.ANNOTATION )
			{
				VOIText textVOI = (VOIText) currentVOI.getCurves().elementAt(0);
	    		Vector3f pt = textVOI.elementAt(0);
	    		String markerID = textVOI.getText();
	    		int id = Integer.valueOf( markerID.substring( 3, markerID.length() ) );				
				
//				System.err.println( markerID + " " + id );
				if ( (id >= 0) && (id < numPoints) )
				{
					left_right_markers[ id ] = pt;
				}
			}
		}
		
		numPairs = numPoints/2;

		JPanel panel = new JPanel(new GridLayout(numPairs, 3));
		panel.setForeground(Color.black);
		
		
		pairFields = new JTextField[numPairs][2];
		for ( int i = 0; i < numPairs; i++ )
		{
			panel.add( new JLabel( "Enter pair: " ) );
			pairFields[i][0] = new JTextField(5);
			pairFields[i][0].addActionListener(this);
			pairFields[i][0].addFocusListener(this);
			panel.add(pairFields[i][0]);
			pairFields[i][1] = new JTextField(5);
			pairFields[i][1].addFocusListener(this);
			panel.add(pairFields[i][1]);
		}
		saveButton = new JButton("Save Lattice");
		saveButton.addActionListener(this);
		
		getContentPane().add(panel, BorderLayout.NORTH);
		getContentPane().add( saveButton, BorderLayout.CENTER );
		okCancelPanel = gui.buildOKCancelPanel();
		getContentPane().add(okCancelPanel, BorderLayout.SOUTH);

		pack();
		setResizable(true);

		System.gc();

	}
	
    public void focusGained(FocusEvent event) { }

    public void focusLost(FocusEvent event) 
    { 
    	setVariables();
    }
    
    
    
    

    
	public static void interpolateLattice( ModelImage image, VOI lattice )
	{
		// Assume image is isotropic (square voxels).
		if ( lattice.getCurves().size() != 2 )
		{
			return;
		}
		VOIContour left = (VOIContour) lattice.getCurves().elementAt(0);
		VOIContour right = (VOIContour) lattice.getCurves().elementAt(1);
		if ( left.size() != right.size() )
		{
			return;
		}
		
		VOIContour center = new VOIContour(false);
		for ( int i = 0; i < left.size(); i++ )
		{
			Vector3f centerPt = Vector3f.add(left.elementAt(i), right.elementAt(i) );
			centerPt.scale(0.5f);
			center.add(centerPt);
		}

		Vector<Vector3f> centerPositions = new Vector<Vector3f>();
		Vector<Vector3f> centerTangents = new Vector<Vector3f>();
		smoothCurve( image, center, 1, centerPositions, centerTangents );

		Vector<Vector3f> leftPositions = new Vector<Vector3f>();
		Vector<Vector3f> leftTangents = new Vector<Vector3f>();
		smoothCurve( image, left, 1, leftPositions, leftTangents );

		Vector<Vector3f> rightPositions = new Vector<Vector3f>();
		Vector<Vector3f> rightTangents = new Vector<Vector3f>();
		smoothCurve( image, right, 1, rightPositions, rightTangents );

		Vector<Float> wormDiameters = new Vector<Float>();
		Vector<Vector3f> rightVectors = new Vector<Vector3f>();
		Vector<Vector3f> upVectors = new Vector<Vector3f>();

		
		float centerLength = 0;
		for ( int i = 1; i < centerPositions.size(); i++ )
		{
			centerLength += centerPositions.elementAt(i).distance( centerPositions.elementAt(i-1) );
		}
		
		float leftLength = 0;
		for ( int i = 1; i < leftPositions.size(); i++ )
		{
			leftLength += leftPositions.elementAt(i).distance( leftPositions.elementAt(i-1) );
		}
		
		float rightLength = 0;
		for ( int i = 1; i < rightPositions.size(); i++ )
		{
			rightLength += rightPositions.elementAt(i).distance( rightPositions.elementAt(i-1) );
		}
		System.err.println( centerPositions.size() + " " + leftPositions.size() + " " + rightPositions.size() );
		System.err.println( centerLength + " " + leftLength + " " + rightLength );

		Vector3f rightDir = Vector3f.sub( rightPositions.elementAt(0), leftPositions.elementAt(0) );		
		float diameter = rightDir.normalize();
		wormDiameters.add(diameter);
		rightVectors.add(rightDir);
		
		centerTangents.elementAt(0).normalize();
		Vector3f upDir = Vector3f.cross( rightDir, centerTangents.elementAt(0) );
		upDir.normalize();
		upVectors.add(upDir);
		
		float distance = 0;
		for ( int i = 1; i < centerPositions.size(); i++ )
		{
			distance += centerPositions.elementAt(i).distance( centerPositions.elementAt(i-1) );
			float t = distance / centerLength;
			Vector3f leftPt = getPosition( leftPositions, t, leftLength );
			Vector3f rightPt = getPosition( rightPositions, t, rightLength );
			


			rightDir = Vector3f.sub( rightPt, leftPt );		
			diameter = rightDir.normalize();
			wormDiameters.add(diameter);
			rightVectors.add(rightDir);
			
			centerTangents.elementAt(i).normalize();
			upDir = Vector3f.cross( rightDir, centerTangents.elementAt(i) );
			upDir.normalize();
			upVectors.add(upDir);
		}

		short sID = (short)(image.getVOIs().getUniqueID());
		VOI samplingPlanes = new VOI(sID, "samplingPlanes");
		for ( int i = 0; i < centerPositions.size(); i++ )
		{
	        Vector3f rkEye = centerPositions.elementAt(i);
	        Vector3f rkRVector = rightVectors.elementAt(i);
	        Vector3f rkUVector = upVectors.elementAt(i);
	        Vector3f rkDVector = centerTangents.elementAt(i);
	        diameter = wormDiameters.elementAt(i);
	        float width = diameter / 2f;
	        float height = diameter / 3.0f;
	        
	        Matrix4f mat = new Matrix4f(
	                                     rkRVector.X,
	                                     rkUVector.X,
	                                     rkDVector.X,
	                                     0.0f,
	                                     rkRVector.Y,
	                                     rkUVector.Y,
	                                     rkDVector.Y,
	                                     0.0f,
	                                     rkRVector.Z,
	                                     rkUVector.Z,
	                                     rkDVector.Z,
	                                     0.0f,
	                                     -rkRVector.dot(rkEye),
	                                     -rkUVector.dot(rkEye),
	                                     -rkDVector.dot(rkEye),
	                                     1.0f );

	        mat.inverse();

	        Vector4f[] corners = new Vector4f[4];
	        corners[0] = new Vector4f(-1,-1, 0, 1);
	        corners[1] = new Vector4f( 1,-1, 0, 1);
	        corners[2] = new Vector4f( 1, 1, 0, 1);
	        corners[3] = new Vector4f(-1, 1, 0, 1);
	        Vector4f[] output = new Vector4f[4];
	        for ( int j = 0; j < 4; j++ )
	        {
	        	corners[j].X *= width;
	        	corners[j].Y *= height;
	        	output[j] =  mat.multLeft( corners[j] );
//	        	System.err.println( corners[j] + "    =>    " + output[j] );
	        }
	        
			VOIContour kBox = new VOIContour(true);
			for ( int j = 0; j < 4; j++ )
			{
				kBox.addElement( output[j].X, output[j].Y, output[j].Z );
			}
			kBox.update( new ColorRGBA(0,0,1,1) );			
			samplingPlanes.importCurve(kBox);
		}
		image.registerVOI(samplingPlanes);
	}

    public static void smoothCurve( ModelImage image, VOIContour curve, float stepSize, 
    		Vector<Vector3f> curvePositions, Vector<Vector3f> curveTangents )
    {

		short sID = (short)(image.getVOIs().getUniqueID());
		VOI originalPoints = new VOI(sID, "originalPoints", VOI.POINT, .5f );
		originalPoints.setCurveType( VOI.POINT );
		
		// 1. Calculate totalArcLength:
    	float totalArcLength = 0;
		float[] xPoints = new float[curve.size()];
		float[] yPoints = new float[curve.size()];
		float[] zPoints = new float[curve.size()];
		for ( int i = 0; i < curve.size(); i++ )
		{
			if ( i > 0 )
			{
				totalArcLength += curve.elementAt(i).distance( curve.elementAt(i-1) );
			}
			xPoints[i] = curve.elementAt(i).X;
			yPoints[i] = curve.elementAt(i).Y;
			zPoints[i] = curve.elementAt(i).Z;
			originalPoints.importPoint( new Vector3f( xPoints[i], yPoints[i], zPoints[i] ) );
		}		

		// 2 Smooth points:
		AlgorithmArcLength arcLength = new AlgorithmArcLength(xPoints, yPoints, zPoints);
		float totalL = arcLength.getTotalArcLength();
		int interpolationPts = (int)((totalL + 5) / stepSize);

		AlgorithmBSmooth smoothAlgo = new AlgorithmBSmooth(image, originalPoints, interpolationPts, true);
		smoothAlgo.run();
		//this is the result b-spline curve
		VOI resultVOI = smoothAlgo.getResultVOI();
		Vector<VOIBase> contours = resultVOI.getCurves();
		int nPoints = contours.size();
		float[] xSmoothedPoints = new float[nPoints+5];
		float[] ySmoothedPoints = new float[nPoints+5];
		float[] zSmoothedPoints = new float[nPoints+5];

		//		System.err.println( nPoints );

		Vector3f point = ((VOIPoint)contours.get(0)).exportPoint();
		xSmoothedPoints[0] = point.X;
		ySmoothedPoints[0] = point.Y;
		zSmoothedPoints[0] = point.Z;

		xSmoothedPoints[1] = point.X;
		ySmoothedPoints[1] = point.Y;
		zSmoothedPoints[1] = point.Z;

		Vector<Vector3f> contour = new Vector<Vector3f>();
		for (int i = 0; i < nPoints; i++) {
			point = ((VOIPoint)contours.get(i)).exportPoint();
			contour.add(new Vector3f(point));
			xSmoothedPoints[i + 2] = point.X;
			ySmoothedPoints[i + 2] = point.Y;
			zSmoothedPoints[i + 2] = point.Z;  
		}

		point = ((VOIPoint)contours.get(nPoints-1)).exportPoint();
		xSmoothedPoints[nPoints + 2] = point.X;
		ySmoothedPoints[nPoints + 2] = point.Y;
		zSmoothedPoints[nPoints + 2] = point.Z;

		xSmoothedPoints[nPoints + 3] = point.X;
		ySmoothedPoints[nPoints + 3] = point.Y;
		zSmoothedPoints[nPoints + 3] = point.Z;

		xSmoothedPoints[nPoints + 4] = point.X;
		ySmoothedPoints[nPoints + 4] = point.Y;
		zSmoothedPoints[nPoints + 4] = point.Z;



		//alg to get tangent vector
		AlgorithmBSpline bSplineAlgo = smoothAlgo.getbSplineAlgo();

		Vector<Vector3f> positions = new Vector<Vector3f>();
		Vector<Vector3f> tangents = new Vector<Vector3f>();
		for ( int i = 0; i < nPoints+1; i++ )
		{
			float floatIndex = i+2;
			positions.add( bSplineAlgo.bSplineJetXYZ(0, floatIndex, xSmoothedPoints, ySmoothedPoints, zSmoothedPoints) );
			tangents.add(bSplineAlgo.bSplineJetXYZ(1, floatIndex, xSmoothedPoints, ySmoothedPoints, zSmoothedPoints) );
		}

		bSplineAlgo = null;
		xSmoothedPoints = null;
		ySmoothedPoints = null;
		zSmoothedPoints = null;

		// 3 Interpolate points, tangents so evenly spaced:
		curvePositions.add( new Vector3f( positions.elementAt(0) ));
		curveTangents.add( new Vector3f( tangents.elementAt(0) ));

		Vector3f currentPoint = new Vector3f( positions.elementAt(0) );
		Vector3f currentTangent = new Vector3f( tangents.elementAt(0) );

		System.err.println( positions.size() );
		//		System.err.println( 0 + " " + positions.elementAt(0) );
		for ( int i = 1; i < positions.size(); i++ )
		{
			Vector3f nextPoint  = new Vector3f( positions.elementAt(i) );
			Vector3f nextTangent  = new Vector3f( tangents.elementAt(i) );
			Vector3f direction = Vector3f.sub(nextPoint, currentPoint);
			direction.normalize();
			direction.scale(stepSize);

			float distance = nextPoint.distance(currentPoint);
			while ( distance >= stepSize )
			{
				Vector3f newPoint = Vector3f.add(currentPoint, direction );
				float interpFactor = newPoint.distance(currentPoint) / nextPoint.distance(currentPoint);

				Vector3f tangent1 = Vector3f.scale( 1 - interpFactor, currentTangent );
				Vector3f tangent2 = Vector3f.scale(     interpFactor, nextTangent );
				Vector3f newTangent = Vector3f.add( tangent1, tangent2 );

				curvePositions.add( new Vector3f(newPoint));
				curveTangents.add( new Vector3f(newTangent));
				currentPoint.copy(newPoint);
				currentTangent.copy(newTangent);
				distance = nextPoint.distance(currentPoint);
			}
		}
		System.err.println( positions.size() );
    }
    
    public static Vector3f getPosition( Vector<Vector3f> points, float t, float totalLength )
    {
    	float distance = 0;
    	for ( int i = 1; i < points.size(); i++ )
    	{
    		float currentT = distance / totalLength;
    		float currentDistance = points.elementAt(i).distance( points.elementAt(i-1) );
    		float nextT = (distance + currentDistance) / totalLength;
    		if ( (t >= currentT) && (t <= nextT) )
    		{
    			float interpFactor = (t - currentT) / currentDistance;
    			Vector3f posM1 = new Vector3f(points.elementAt(i-1));
    			posM1.scale( 1 - interpFactor );
    			Vector3f pos = new Vector3f(points.elementAt(i));
    			pos.scale( interpFactor );
    			return posM1.add(pos);
    		}
    		distance += currentDistance;
    	}  		
    	return points.lastElement();
    }
}
