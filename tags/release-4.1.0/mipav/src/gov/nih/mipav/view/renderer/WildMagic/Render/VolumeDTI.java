package gov.nih.mipav.view.renderer.WildMagic.Render;

import java.util.Iterator;
import java.util.HashMap;
import java.util.Vector;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.ViewJProgressBar;
import WildMagic.LibFoundation.Curves.BSplineCurve3f;
import WildMagic.LibFoundation.Mathematics.*;
import WildMagic.LibGraphics.Effects.*;
import WildMagic.LibGraphics.Rendering.*;
import WildMagic.LibGraphics.SceneGraph.*;
import WildMagic.LibGraphics.Shaders.*;
import WildMagic.LibGraphics.Surfaces.TubeSurface;

/** Displays the Diffusion Tensor tracts in the VolumeViewer.
 * @see VolumeObject.java
 * @see GPUVolumeRender.java
 */
public class VolumeDTI extends VolumeObject
{

    /** Creates a new VolumeDTI object.
     * @param kImageA, the VolumeImage containing shared data and textures for
     * rendering.
     * @param kTranslate, translation in the scene-graph for this object.
     * @param fX, the size of the volume in the x-dimension (extent * resolutions)
     * @param fY, the size of the volume in the y-dimension (extent * resolutions)
     * @param fZ, the size of the volume in the z-dimension (extent * resolutions)
     */
    public VolumeDTI( VolumeImage kVolumeImage, Vector3f kTranslate, float fX, float fY, float fZ )
    {
        super(kVolumeImage,kTranslate,fX,fY,fZ);

        m_iDimX = m_kVolumeImageA.GetImage().getExtents()[0];
        m_iDimY = m_kVolumeImageA.GetImage().getExtents()[1];
        m_iDimZ = m_kVolumeImageA.GetImage().getExtents()[2];
        m_fScale = 1.0f/(float)(Math.max(m_iDimX,Math.max(m_iDimY,m_iDimZ)));
        m_iLen = m_iDimX*m_iDimY*m_iDimZ;
        
        m_kScene = new Node();
        m_kVertexColor3Shader = new VertexColor3Effect();
        m_kAlpha = new AlphaState();
        m_kAlpha.BlendEnabled = true;
        m_kAlpha.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE_MINUS_DST_COLOR;
        m_kAlpha.DstBlend = AlphaState.DstBlendMode.DBF_ONE;
    }
    
    /**
     * PreRender the object, for embedding in the ray-cast volume.
     * @param kRenderer, the OpenGLRenderer object.
     * @param kCuller, the Culler object.
     */
    public void PreRender(Renderer kRenderer, Culler kCuller )
    {
        if ( !m_bDisplay )
        {
            return;
        }
        if ( m_bDisplayAllEllipsoids )
        {
            DisplayAllEllipsoids( m_kVolumeImageA.GetImage(), kRenderer);
        }
        else
        {
            DisplayTract(m_kVertexColor3Shader, kRenderer);
        }
    }

    /**
     * Render the object.
     * @param kRenderer, the OpenGLRenderer object.
     * @param kCuller, the Culler object.
     */
    public void Render( Renderer kRenderer, Culler kCuller )
    {
        if ( !m_bDisplay )
        {
            return;
        }
        AlphaState aTemp = kRenderer.GetAlphaState();
        kRenderer.SetAlphaState(m_kAlpha);

        if ( m_bDisplayAllEllipsoids )
        {
            DisplayAllEllipsoids( m_kVolumeImageA.GetImage(), kRenderer);
        }
        else if ( m_bDisplayEllipsoids )
        {
            DisplayEllipsoids( m_kVolumeImageA.GetImage(), kRenderer );
        }
        else if ( m_bDisplayAllCylinders )
        {
            DisplayAllCylinders( m_kVolumeImageA.GetImage(), kRenderer);
        }
        else if ( m_bDisplayCylinders )
        {
            DisplayCylinders( m_kVolumeImageA.GetImage(), kRenderer );
        }
        else if ( m_bDisplayTubes ) {
        	DisplayTubes(m_kVolumeImageA.GetImage(), kRenderer);
        }
        else 
        {
            DisplayTract(null, kRenderer );
        }
        kRenderer.SetAlphaState(aTemp);
    }

    /**
     * Sets the light for the EllipsoidsShader.
     * @param kLightType, the name of the light to set (Light0, Light1, etc.)
     * @param afType, the type of light (Ambient = 0, Directional = 1, Point = 2, Spot = 3).
     */
    public void SetLight( String kLightType, float[] afType )
    {
        if ( m_kAllEllipsoidsShader != null )
        {
            m_kAllEllipsoidsShader.SetLight(kLightType, afType);
        }
        if ( m_kLightShader != null )
        {
            m_kLightShader.SetLight(kLightType, afType);
        }
    }


    /** Add a polyline to the display. Used to display fiber tract bundles.
     * @param kLine, new polyline to display.
     * @param iGroup, the group the polyline belongs to.
     */
    public void addPolyline( Polyline kLine, int iGroup )
    {
        if ( kLine == null )
        {
            return;
        }
        if ( m_kTracts == null )
        {
            m_kTracts = new HashMap<Integer,Node>();
            m_kEllipsoids = new HashMap<Integer,Vector<int[]>>();
            m_kCylinders = new HashMap<Integer,Vector<int[]>>();
            m_kShaders = new HashMap<Integer,ShaderEffect>();
            m_kEllipseConstantColor = new HashMap<Integer,ColorRGB>();
        }
        if ( m_kTubes == null ) {
            m_kTubes = new HashMap<Integer,Node>();
            m_kTubeColors = new HashMap<Integer, Integer>();
        }        
        if ( m_iMaxGroups < iGroup )
        {
            m_iMaxGroups = iGroup;
        }

        int[] aiEllipsoids = new int[kLine.VBuffer.GetVertexQuantity()];
        int[] aiCylinders = new int[kLine.VBuffer.GetVertexQuantity()];

        for ( int i = 0; i < kLine.VBuffer.GetVertexQuantity(); i++ )
        {
            int iX = (int)((kLine.VBuffer.GetPosition3fX(i) +.5f) * m_iDimX);
            int iY = (int)((kLine.VBuffer.GetPosition3fY(i) +.5f) * m_iDimY);
            int iZ = (int)((kLine.VBuffer.GetPosition3fZ(i) +.5f) * m_iDimZ);
            int iIndex = iZ * m_iDimY * m_iDimX + iY * m_iDimX + iX;

            if ( m_kEigenVectors != null )
            {
                if (  m_kEigenVectors.get( new Integer(iIndex) ) != null )
                {
                    aiEllipsoids[i] = iIndex;
                    aiCylinders[i] = iIndex;
                }
                else
                {
                    aiEllipsoids[i] = -1;
                    aiCylinders[i] = -1;
                }
            }
        }

        kLine.Local.SetScale( new Vector3f( m_fX, m_fY, m_fZ ) );

        Node kTractNode = null;
        Node kTubeNode = null;
        
        Integer iIGroup = new Integer(iGroup);
        if ( m_kTracts.containsKey( iIGroup ) )
        {
            kTractNode = m_kTracts.get(iIGroup);
            kTractNode.AttachChild(kLine);
            kTractNode.UpdateGS();
            kTractNode.UpdateRS();

            Vector<int[]> kEllipseVector = m_kEllipsoids.get(iIGroup);
            kEllipseVector.add(aiEllipsoids);
            Vector<int[]> kCylinderVector = m_kCylinders.get(iIGroup);
            kCylinderVector.add(aiCylinders);
        }
        
        if ( m_kTubes.containsKey( iIGroup ) )
        {
        	kTubeNode = m_kTubes.get(iIGroup);
            kTubeNode.AttachChild(createTube(kLine));
            kTubeNode.UpdateGS();
            kTubeNode.UpdateRS();
            m_kTubeColors.put(new Integer(iIGroup), new Integer(centerIndex));
        }
        
        if ( kTractNode == null )
        {
            kTractNode = new Node();
            kTractNode.AttachChild(kLine);
            kTractNode.UpdateGS();
            kTractNode.UpdateRS();
            m_kTracts.put( new Integer(iIGroup), kTractNode );
 
            Vector<int[]> kEllipseVector = new Vector<int[]>();
            kEllipseVector.add(aiEllipsoids);
            m_kEllipsoids.put( new Integer(iIGroup), kEllipseVector );
            
            Vector<int[]> kCylinderVector = new Vector<int[]>();
            kCylinderVector.add(aiCylinders);
            m_kCylinders.put( new Integer(iIGroup), kCylinderVector );

            String kShaderName = new String( "ConstantColor" );
            VertexColor3Effect kPolylineShader = new VertexColor3Effect( kShaderName, true );
            m_kShaders.put( new Integer(iIGroup), kPolylineShader );
        }
        
        if ( kTubeNode == null ) {
        	 kTubeNode = new Node();
             kTubeNode.AttachChild(createTube(kLine));
             kTubeNode.UpdateGS();
             kTubeNode.UpdateRS();
             m_kTubes.put( new Integer(iIGroup), kTubeNode );
             m_kTubeColors.put(new Integer(iIGroup), new Integer(centerIndex));
        }
    }

    /**
     * Generate the tube streamline from the given polyline.
     * @param kTract  polyline of the medial path.
     * @return  kTube Tube surface generated. 
     */
    public TubeSurface createTube(Polyline kTract) {
    	TubeSurface kTube;
    	int iNumCtrlPoints = kTract.VBuffer.GetVertexQuantity();
        Vector3f[] akCtrlPoint = new Vector3f[iNumCtrlPoints];
        for ( int idx =0; idx < iNumCtrlPoints; idx++ ) {
        	akCtrlPoint[idx] = kTract.VBuffer.GetPosition3(idx);
        }
       
        int iDegree = 2;
        BSplineCurve3f m_pkSpline = new BSplineCurve3f(iNumCtrlPoints,akCtrlPoint,iDegree,
            false,true);
        
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetNChannels(3);
        kAttr.SetTChannels(0,3);
        kAttr.SetCChannels(0,4);
        
        Vector2f kUVMin = new Vector2f(0.0f,0.0f);
        Vector2f kUVMax = new Vector2f(1.0f,1.0f);
        kTube = new TubeSurface(m_pkSpline,0.025f, false,Vector3f.UNIT_Z,
        		iNumCtrlPoints,8,kAttr,false,false,kUVMin,kUVMax);
        return kTube;
    }
    
    /** 
     * Removes the specified polyline tract group.
     * @param iGroup, the group of polylines to remove.
     */
    public void removePolyline( int iGroup )
    {
        Integer kGroup = new Integer(iGroup);
        if ( m_kTracts == null || !m_kTracts.containsKey(iGroup) )
        {
            return;
        }
        Node kTractNode = m_kTracts.remove(kGroup);
        if ( kTractNode == null )
        {
            return;
        }
        for ( int i = 0; i < kTractNode.GetQuantity(); i++ )
        {
            Polyline kTract = (Polyline)kTractNode.DetachChildAt(i);
            if ( kTract != null )
            { 
                kTract.DetachAllEffects();
                kTract.dispose();
            }
        }
        kTractNode.UpdateGS();
        kTractNode.UpdateRS();
        kTractNode.dispose();
        kTractNode = null;
        if ( m_kTracts.size() == 0 )
        {
            m_kTracts = null;
        }

        if ( m_kTubes == null ||  !m_kTubes.containsKey(iGroup) )
        {
            return;
        }
        Node kTubeNode = m_kTubes.remove(kGroup);
        m_kTubeColors.remove(kGroup);
        if ( kTubeNode == null )
        {
            return;
        }
        for ( int i = 0; i < kTubeNode.GetQuantity(); i++ )
        {
        	TubeSurface kTube = (TubeSurface)kTubeNode.DetachChildAt(i);
            if ( kTube != null )
            { 
            	kTube.DetachAllEffects();
            	kTube.dispose();
            }
        }
        kTubeNode.UpdateGS();
        kTubeNode.UpdateRS();
        kTubeNode.dispose();
        kTubeNode = null;
        if ( m_kTubes.size() == 0 )
        {
            m_kTubes = null;
            m_kTubeColors = null;
        }
        
        Vector<int[]> kEllipseVector = m_kEllipsoids.remove(kGroup);
        if ( kEllipseVector != null )
        {
            kEllipseVector.clear();
        }
        
        Vector<int[]> kCylinderVector = m_kCylinders.remove(kGroup);
        if ( kCylinderVector != null )
        {
            kCylinderVector.clear();
        }

        ShaderEffect kShader = m_kShaders.remove(kGroup);
        if ( kShader != null )
        {
            kShader.dispose();
        }
    }
    
    /** Returns if there are tracts to display.
     * @return true if there are tracts currently loaded, false otherwise.
     */
    public boolean GetDisplayTract()
    {
        if ( m_kTracts == null )
        {
            return false;
        }
        return true;
    }

    /** Sets the polyline color for the specified fiber bundle tract group. 
     * @param iGroup, the fiber bundle group to set.
     * @param kColor the new polyline color for the specified fiber bundle tract group. 
     */
    public void setPolylineColor( int iGroup, ColorRGB kColor )
    {
        Integer kKey = new Integer(iGroup);
        ShaderEffect kShader = m_kShaders.get(kKey);
        if ( kShader == null )
        {
            return;
        }
        Program pkProgram = kShader.GetVProgram(0);
        if ( pkProgram == null )
        {
            return;
        }
        if ( kColor == null )
        {
            if ( pkProgram.GetUC("UseConstantColor") != null )
            {
                pkProgram.GetUC("UseConstantColor").SetDataSource(new float[] {0,0,0,0});
            }

            m_kEllipseConstantColor.remove( kKey );
            m_kEllipseConstantColor.put( kKey, null );
        }
        else
        {
            if ( pkProgram.GetUC("ConstantColor") != null )
            {
                pkProgram.GetUC("ConstantColor").SetDataSource(new float[] { kColor.R(), kColor.G(), kColor.B(), 1f } );
            }
            if ( pkProgram.GetUC("UseConstantColor") != null )
            {
                pkProgram.GetUC("UseConstantColor").SetDataSource(new float[] {1,0,0,0});
            }

            m_kEllipseConstantColor.remove( kKey );
            m_kEllipseConstantColor.put( kKey, kColor );
        }
    }

    /** Returns the polyline color for the specified fiber bundle tract group. 
     * @param iGroup, the fiber bundle group to query.
     * @return the polyline color for the specified fiber bundle tract group. 
     */
    public ColorRGB getPolylineColor( int iGroup )
    {
        if ( m_kEllipseConstantColor != null )
        {
            return m_kEllipseConstantColor.get( new Integer(iGroup) );
        }
        return null;
    }

    /** Sets the DTI Image for displaying the tensors as ellipsoids.
     * @param kDTIImage.
     */
    public void setDTIImage( ModelImage kDTIImage )
    {
        ViewJProgressBar kProgressBar = new ViewJProgressBar("Calculating ellipse transforms", "", 0, 100, true);
        
        m_kEigenVectors =
            new HashMap<Integer,Transformation>();
        Matrix3f kMatrix = new Matrix3f();;
        float[] afTensorData = new float[6];
        Matrix3f kEigenValues = new Matrix3f();
        float fLambda1;
        float fLambda2;
        float fLambda3;
        Vector3f kV1 = new Vector3f();
        Vector3f kV2 = new Vector3f();
        Vector3f kV3 = new Vector3f();

        for ( int i = 0; i < m_iLen; i++ )
        {
            boolean bAllZero = true;
            for ( int j = 0; j < 6; j++ )
            {
                afTensorData[j] = kDTIImage.getFloat(i + j*m_iLen);
                if ( afTensorData[j] != 0 )
                {
                    bAllZero = false;
                }
            }
            if ( !bAllZero )
            {
                kMatrix.SetData( afTensorData[0], afTensorData[3], afTensorData[4],
                        afTensorData[3], afTensorData[1], afTensorData[5], 
                        afTensorData[4], afTensorData[5], afTensorData[2] );

                if ( Matrix3f.EigenDecomposition( kMatrix, kEigenValues ) )
                {
                    fLambda1 = kEigenValues.GetData(2,2);
                    fLambda2 = kEigenValues.GetData(1,1);
                    fLambda3 = kEigenValues.GetData(0,0);
                    kMatrix.GetColumn(2,kV1);
                    kMatrix.GetColumn(1,kV2);
                    kMatrix.GetColumn(0,kV3);

                    kV1.Normalize();
                    kV2.Normalize();
                    kV3.Normalize();

                    kMatrix.SetColumn(0,kV1);
                    kMatrix.SetColumn(1,kV2);
                    kMatrix.SetColumn(2,kV3);

                    if ( (fLambda1 == fLambda2) && (fLambda1 == fLambda3) )
                    {}
                    else if ( (fLambda1 > 0) && (fLambda2 > 0) && (fLambda3 > 0) )
                    {
                        Transformation kTransform = new Transformation();
                        kTransform.SetMatrix(new Matrix3f(kMatrix));
                        Vector3f kScale = new Vector3f( fLambda1, fLambda2, fLambda3 );
                        kScale.Normalize();
                        kTransform.SetScale( kScale );
                        m_kEigenVectors.put( new Integer(i), kTransform );
                    }
                }
            }
            if ( (i%(m_iDimX*m_iDimY)) == 0 )
            {
                int iValue = (int)(100 * (float)(i+1)/(float)m_iLen);
                kProgressBar.updateValueImmed( iValue );
            }
        }
        kProgressBar.dispose();

        Integer kKey;
        int iIndex, iX, iY, iZ;
        float fX, fY, fZ;
        Vector3f kScale;
        Transformation kTransform;
        Transformation kTScale = new Transformation();
        Transformation kTEllipse = new Transformation();

        Iterator kIterator = m_kEigenVectors.keySet().iterator();
        while ( kIterator.hasNext() )
        {
            kKey = (Integer)kIterator.next();
            iIndex = kKey.intValue();
            iX = iIndex % m_iDimX;
            iIndex -= iX;
            iIndex /= m_iDimX;

            iY = iIndex % m_iDimY;
            iIndex -= iY;
            iIndex /= m_iDimY;

            iZ = iIndex;

            // reset iIndex:
                iIndex = kKey.intValue();

            fX = (float)(iX)/(float)(m_iDimX);
            fY = (float)(iY)/(float)(m_iDimY);
            fZ = (float)(iZ)/(float)(m_iDimZ);


            kTScale.MakeIdentity();
            kTEllipse.MakeIdentity();

            kTransform = m_kEigenVectors.get(kKey);

            kScale = kTScale.GetScale();
            kScale.scaleEquals( m_fScale );
            kScale.multEquals( kTransform.GetScale() );
            kTScale.SetScale(kScale);

            kTEllipse.SetTranslate( fX - .5f, fY - .5f, fZ - .5f );
            kTEllipse.SetMatrixCopy( kTransform.GetMatrix() );


            kTransform.Product( kTEllipse, kTScale );
        }
        kTEllipse = null;
        kTScale = null;

        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetNChannels(3);

        StandardMesh kSM = new StandardMesh(kAttr);
        kSM.SetInside(true);
        m_kSphere = kSM.Sphere(64,64,1f);
        
        Transformation trans = new Transformation();
        float rotationRedian = 0.5f * (float) Math.PI;

        Matrix3f matrix = new Matrix3f((float)Math.cos(rotationRedian),0.0f,(float)Math.sin(rotationRedian),
                					   0.0f,1.0f,0.0f,
                					   (float)-Math.sin(rotationRedian),0.0f,(float)Math.cos(rotationRedian));

        trans.SetRotate(matrix);
  
        StandardMesh cylinder = new StandardMesh(kAttr);
        kSM.SetInside(true);
        cylinder.SetTransformation(trans);
        m_kCylinder = cylinder.Cylinder(64,64,1.0f,2f,false);
        

        m_kAllEllipsoidsShader = new MipavLightingEffect( );
        
        m_kLightShader = new SurfaceLightingEffect( m_kVolumeImageA );
        m_kLightShader.SetSurfaceTexture(true, false, false);
        
        
        textureEffect = new TextureEffect("Water");
        Texture pkTexture = textureEffect.GetPTexture(0,0);
        pkTexture.SetFilterType(Texture.FilterType.LINEAR_LINEAR);
        pkTexture.SetWrapType(0,Texture.WrapType.REPEAT);
        pkTexture.SetWrapType(1,Texture.WrapType.REPEAT);
        
        m_kEllipseMaterial = new MaterialState();
        m_kEllipseMaterial.Emissive = new ColorRGB(ColorRGB.BLACK);
        m_kEllipseMaterial.Ambient = new ColorRGB(0.24725f,0.2245f,0.0645f);
        m_kEllipseMaterial.Diffuse = new ColorRGB(0.34615f,0.3143f,0.0903f);
        m_kEllipseMaterial.Specular = new ColorRGB(1f,1f,1f);
        m_kEllipseMaterial.Shininess = 32f;
        m_kEllipseMaterial.Alpha = 1f;
        m_kColorEllipse = new ColorRGB(ColorRGB.BLACK);

        m_kTubesMaterial = new MaterialState();
        m_kTubesMaterial.Emissive = new ColorRGB(ColorRGB.BLACK);
        m_kTubesMaterial.Ambient = new ColorRGB(0.24725f,0.2245f,0.0645f);
        m_kTubesMaterial.Diffuse = new ColorRGB(0.34615f,0.3143f,0.0903f);
        m_kTubesMaterial.Specular = new ColorRGB(1f,1f,1f);
        m_kTubesMaterial.Alpha = 1f;
        m_kTubesMaterial.Shininess = 500f;
        
        
        m_kSphere.AttachGlobalState(m_kEllipseMaterial);
        // m_kSphere.AttachEffect(m_kAllEllipsoidsShader);
        m_kSphere.AttachEffect(textureEffect);
        m_kSphere.AttachEffect(m_kLightShader);
        m_kSphere.UpdateRS();
        
        m_kCylinder.AttachGlobalState(m_kEllipseMaterial);
        // m_kCylinder.AttachEffect(m_kAllEllipsoidsShader);
        m_kCylinder.AttachEffect(textureEffect);
        m_kCylinder.AttachEffect(m_kLightShader);
        m_kCylinder.UpdateRS();
    }

    /** Turns on/off displaying the fiber bundle tracts with ellipsoids.
     * @param bDisplay, when true display the tracts with ellipsods.
     */
    public void setDisplayEllipsoids( boolean bDisplay )
    {
        m_bDisplayEllipsoids = bDisplay;
    }

    /** Turns on/off displaying all the ellipsoids.
     * @param bDisplay, when true display all the ellipsods in the volume.
     */
    public void setDisplayAllEllipsoids( boolean bDisplay )
    {
        m_bDisplayAllEllipsoids = bDisplay;
    }

    /** Turns on/off displaying the fiber bundle tracts with cylinders.
     * @param bDisplay, when true display the tracts with cylinders.
     */
    public void setDisplayCylinders( boolean bDisplay )
    {
        m_bDisplayCylinders = bDisplay;
    }

    /** Turns on/off displaying all the cylinders.
     * @param bDisplay, when true display all the cylinders in the volume.
     */
    public void setDisplayAllCylinders( boolean bDisplay )
    {
        m_bDisplayAllCylinders = bDisplay;
    }
    
    /** Turns on/off displaying the fiber bundle tracts with cylinders.
     * @param bDisplay, when true display the tracts with cylinders.
     */
    public void setDisplayTubes( boolean bDisplay )
    {
        m_bDisplayTubes = bDisplay;
    }
    

    /** Set the m_iEllipsoidMod value. 
     * @param iMod, new m_iEllipsoidMod value.
     */
    public void setEllipseMod( int iMod )
    {
        m_iEllipsoidMod = iMod;
    }

    /** Display the DTI volume with ellipsoids at each voxel. The m_iEllipsMod
     * value is used to limit the number of ellipsoids displayed.
     */    
    public void DisplayAllEllipsoids( ModelImage kImage, Renderer kRenderer/*, AlphaState kAlpha */)
    {
        if ( m_kEigenVectors == null )
        {
            return;
        }
        
        //kAlpha.BlendEnabled = true;
        Node kScaleNode = new Node();
        kScaleNode.Local.SetScale( m_fX, m_fY, m_fZ );
        int iCount = 0;
        int iIndex;
        Integer kKey;
        float fR, fG, fB;
        TriMesh kEllipse;
        int iDisplayed = 0;
        Iterator kIterator = m_kEigenVectors.keySet().iterator();
        while ( kIterator.hasNext() )
        {
            kKey = (Integer)kIterator.next();
            if ( (iCount%m_iEllipsoidMod) == 0 )
            {                           
                iIndex = kKey.intValue();                          
                if ( kImage.isColorImage() )
                {
                    fR = kImage.getFloat( iIndex*4 + 1 )/255.0f;
                    fG = kImage.getFloat( iIndex*4 + 2 )/255.0f;
                    fB = kImage.getFloat( iIndex*4 + 3 )/255.0f;
                    m_kColorEllipse.R(fR);
                    m_kColorEllipse.G(fG);
                    m_kColorEllipse.B(fB);
                }
                else
                {
                    fR = kImage.getFloat( iIndex );
                    m_kColorEllipse.R(fR);
                    m_kColorEllipse.G(fR);
                    m_kColorEllipse.B(fR);
                }

                kEllipse = m_kSphere;
                kEllipse.Local = m_kEigenVectors.get(kKey);
                
                m_kEllipseMaterial.Ambient = m_kColorEllipse;
                m_kEllipseMaterial.Diffuse = m_kColorEllipse;

                kScaleNode.SetChild(0, kEllipse);
                m_kScene.SetChild(0,kScaleNode);
                m_kScene.UpdateGS();
                m_kScene.DetachChild(kScaleNode);
                kScaleNode.DetachChild(kEllipse);
                
                kRenderer.Draw(kEllipse);
                iDisplayed++;
            }
            iCount++;
        }
    }

    /** Display a fiber bundle tract with ellipsoids at each voxel.
     */    
    private void DisplayEllipsoids( ModelImage kImage, Renderer kRenderer )
    {
        if ( m_kEllipsoids == null )
        {
            return;
        }
        //kAlpha.BlendEnabled = true;
        Node kScaleNode = new Node();
        kScaleNode.Local.SetScale( m_fX, m_fY, m_fZ );
        Integer kKey;
        Vector<int[]> kEllipseVector;
        int[] aiEllipsoids;
        int iIndex;
        ColorRGB kColor;
        float fR,fG,fB;
        TriMesh kEllipse;        
        Iterator kIterator = m_kEllipsoids.keySet().iterator();
        while ( kIterator.hasNext() )
        {
            kKey = (Integer)kIterator.next();
            kEllipseVector = m_kEllipsoids.get(kKey);
            for ( int i = 0; i < kEllipseVector.size(); i++ )
            {
                aiEllipsoids = kEllipseVector.get(i);
                for ( int j = 0; j < aiEllipsoids.length; j++ )
                {
                    if ( aiEllipsoids[j] != -1 )
                    {
                        iIndex = aiEllipsoids[j];
                        Integer kIndex = new Integer(iIndex);
                        kColor = m_kEllipseConstantColor.get(kIndex);
                       
                        if ( kColor != null )
                        {
                            m_kColorEllipse = kColor;
                        }
                        else
                        {
                            if ( kImage.isColorImage() )
                            {
                                fR = kImage.getFloat( iIndex*4 + 1 )/255.0f;
                                fG = kImage.getFloat( iIndex*4 + 2 )/255.0f;
                                fB = kImage.getFloat( iIndex*4 + 3 )/255.0f;
                                m_kColorEllipse.R(fR);
                                m_kColorEllipse.G(fG);
                                m_kColorEllipse.B(fB);
                            }
                            else
                            {
                                fR = kImage.getFloat( iIndex );
                                m_kColorEllipse.R(fR);
                                m_kColorEllipse.G(fR);
                                m_kColorEllipse.B(fR);
                            }
                        }
                        
                        kEllipse = m_kSphere;
                        kEllipse.Local = m_kEigenVectors.get(kIndex);
                
                        m_kEllipseMaterial.Ambient = m_kColorEllipse;
                        m_kEllipseMaterial.Diffuse = m_kColorEllipse;
                        kScaleNode.SetChild(0, kEllipse);
                        m_kScene.SetChild(0,kScaleNode);
                        m_kScene.UpdateGS();
                        m_kScene.DetachChild(kScaleNode);
                        kScaleNode.DetachChild(kEllipse);
                        kRenderer.Draw(kEllipse);
                    }
                }
            }
        }
    }
    
    /** Display the DTI volume with ellipsoids at each voxel. The m_iEllipsMod
     * value is used to limit the number of ellipsoids displayed.
     */    
    public void DisplayAllCylinders( ModelImage kImage, Renderer kRenderer/*, AlphaState kAlpha */)
    {
        if ( m_kEigenVectors == null )
        {
            return;
        }
        
        //kAlpha.BlendEnabled = true;
        Node kScaleNode = new Node();
        kScaleNode.Local.SetScale( m_fX, m_fY, m_fZ );
        int iCount = 0;
        int iIndex;
        Integer kKey;
        float fR, fG, fB;
        TriMesh kCylinder;
        int iDisplayed = 0;
        Iterator kIterator = m_kEigenVectors.keySet().iterator();
        while ( kIterator.hasNext() )
        {
            kKey = (Integer)kIterator.next();
            if ( (iCount%m_iEllipsoidMod) == 0 )
            {                           
                iIndex = kKey.intValue();                          
                if ( kImage.isColorImage() )
                {
                    fR = kImage.getFloat( iIndex*4 + 1 )/255.0f;
                    fG = kImage.getFloat( iIndex*4 + 2 )/255.0f;
                    fB = kImage.getFloat( iIndex*4 + 3 )/255.0f;
                    m_kColorEllipse.R(fR);
                    m_kColorEllipse.G(fG);
                    m_kColorEllipse.B(fB);
                }
                else
                {
                    fR = kImage.getFloat( iIndex );
                    m_kColorEllipse.R(fR);
                    m_kColorEllipse.G(fR);
                    m_kColorEllipse.B(fR);
                }

                kCylinder = m_kCylinder;
                kCylinder.Local = m_kEigenVectors.get(kKey);
                
                m_kEllipseMaterial.Ambient = m_kColorEllipse;
                m_kEllipseMaterial.Diffuse = m_kColorEllipse;

                kScaleNode.SetChild(0, kCylinder);
                m_kScene.SetChild(0,kScaleNode);
                m_kScene.UpdateGS();
                m_kScene.DetachChild(kScaleNode);
                kScaleNode.DetachChild(kCylinder);
                
                kRenderer.Draw(kCylinder);
                iDisplayed++;
            }
            iCount++;
        }
    }

    
    /** Display a fiber bundle tract with cylinders at each voxel.
     */    
    private void DisplayCylinders( ModelImage kImage, Renderer kRenderer )
    {
        if ( m_kCylinders == null )
        {
            return;
        }
        //kAlpha.BlendEnabled = true;
        Node kScaleNode = new Node();
        kScaleNode.Local.SetScale( m_fX, m_fY, m_fZ );
        Integer kKey;
        Vector<int[]> kCylinderVector;
        int[] aiCylinders;
        int iIndex;
        ColorRGB kColor;
        float fR,fG,fB;
        TriMesh kCylinder;        
        Iterator kIterator = m_kCylinders.keySet().iterator();
        while ( kIterator.hasNext() )
        {
            kKey = (Integer)kIterator.next();
            kCylinderVector = m_kCylinders.get(kKey);
            for ( int i = 0; i < kCylinderVector.size(); i++ )
            {
                aiCylinders = kCylinderVector.get(i);
                for ( int j = 0; j < aiCylinders.length; j++ )
                {
                    if ( aiCylinders[j] != -1 )
                    {
                        iIndex = aiCylinders[j];
                        Integer kIndex = new Integer(iIndex);
                        kColor = m_kEllipseConstantColor.get(kIndex);
                       
                        if ( kColor != null )
                        {
                            m_kColorEllipse = kColor;
                        }
                        else
                        {
                            if ( kImage.isColorImage() )
                            {
                                fR = kImage.getFloat( iIndex*4 + 1 )/255.0f;
                                fG = kImage.getFloat( iIndex*4 + 2 )/255.0f;
                                fB = kImage.getFloat( iIndex*4 + 3 )/255.0f;
                                m_kColorEllipse.R(fR);
                                m_kColorEllipse.G(fG);
                                m_kColorEllipse.B(fB);
                            }
                            else
                            {
                                fR = kImage.getFloat( iIndex );
                                m_kColorEllipse.R(fR);
                                m_kColorEllipse.G(fR);
                                m_kColorEllipse.B(fR);
                            }
                        }
                        
                        kCylinder = m_kCylinder;
                        kCylinder.Local = m_kEigenVectors.get(kIndex);
                
                        m_kEllipseMaterial.Ambient = m_kColorEllipse;
                        m_kEllipseMaterial.Diffuse = m_kColorEllipse;
                        kScaleNode.SetChild(0, kCylinder);
                        m_kScene.SetChild(0,kScaleNode);
                        m_kScene.UpdateGS();
                        m_kScene.DetachChild(kScaleNode);
                        kScaleNode.DetachChild(kCylinder);
                        kRenderer.Draw(kCylinder);
                    }
                }
            }
        }
    }   
 
    
    public void setCenterIndex(int index) {
    	centerIndex = index;
    }
    
    /** Displays a tube fiber bundle tract with the given shader attached.
     * @param kInputStader, shader to apply to the tube.
     */    
    private void DisplayTubes( ModelImage kImage, Renderer kRenderer )
    {
        Node kScaleNode = new Node();
        Node kTubeNode;
        kScaleNode.Local.SetScale( m_fX, m_fY, m_fZ );
        Integer iKey;
        int iIndex;
        float fR,fG,fB;        
        Iterator iIterator = m_kTubes.keySet().iterator();

        TubeSurface kTube;
        
        while ( iIterator.hasNext() )
        {
            iKey = (Integer)iIterator.next();
            
            kTubeNode = m_kTubes.get(iKey);
            
                
                kTube = (TubeSurface)kTubeNode.GetChild(0);
                           
                
                iIndex = m_kTubeColors.get(iKey);
                ColorRGB kColor1;
                if ( kImage.isColorImage() )
                {
                    fR = kImage.getFloat( iIndex*4 + 1 )/255.0f;
                    fG = kImage.getFloat( iIndex*4 + 2 )/255.0f;
                    fB = kImage.getFloat( iIndex*4 + 3 )/255.0f;
                    kColor1 = new ColorRGB(fR, fG, fB);
                }
                else
                {
                    fR = kImage.getFloat( iIndex );
                    kColor1 = new ColorRGB(fR, fR, fR);
                }                
                
                
                
                kTube.AttachGlobalState(m_kTubesMaterial);
                
                kTube.AttachEffect(textureEffect);
                kTube.AttachEffect(m_kLightShader);
                
                kTube.UpdateRS();
                
                kTube.UpdateSurface();
                kTube.VBuffer.Release();
                
                m_kTubesMaterial.Ambient = kColor1;
                m_kTubesMaterial.Diffuse = kColor1;
                m_kTubesMaterial.Emissive = new ColorRGB(ColorRGB.BLACK);
                m_kTubesMaterial.Specular = new ColorRGB(ColorRGB.WHITE); 
                m_kTubesMaterial.Alpha = 1.0f;
                m_kTubesMaterial.Shininess = 100f;
               
                
                kScaleNode.SetChild(0, kTube);
                m_kScene.SetChild(0,kScaleNode);
                m_kScene.UpdateGS();
                m_kScene.DetachChild(kScaleNode);
                kScaleNode.DetachChild(kTube);
                kRenderer.Draw(kTube);
            
        }
    }
    
    /** Displays a polyline fiber bundle tract with the given shader attached.
     * @param kInputStader, shader to apply to the polyline.
     */    
    private void DisplayTract( ShaderEffect kInputShader, Renderer kRenderer )
    {
        Iterator kIterator = m_kTracts.keySet().iterator();
        Integer iKey;
        Node kTractNode;
        Polyline kTract;
        ShaderEffect kShader;
        
        //kAlpha.BlendEnabled = true;
        while ( kIterator.hasNext() )
        {
            iKey = (Integer)kIterator.next();
            kTractNode = m_kTracts.get(iKey);
            kShader = kInputShader;
            if ( kShader == null )
            {
                kShader = m_kShaders.get(iKey);
            }
            for ( int i = 0; i < kTractNode.GetQuantity(); i++ )
            {
                kTract = (Polyline)kTractNode.GetChild(i);
                kTract.DetachAllEffects();
                kTract.AttachEffect( kShader );
                
                m_kScene.SetChild(0,kTract);
                m_kScene.UpdateGS();
                kRenderer.Draw(kTract);
                m_kScene.DetachChild(kTract);
                kTract.DetachEffect( kShader );
            }
        }
    }

    /**
     * memory cleanup.
     */
    public void dispose()
    {
        if ( m_kTracts != null )
        {
            for ( int i = 0; i < m_iMaxGroups; i++ )
            {
                removePolyline(i);
            }
        }

        m_kTracts = null;
        m_kShaders = null;
        m_kEllipsoids = null;
        m_kEigenVectors = null;
        m_kEllipseConstantColor = null;

        if ( m_kAllEllipsoidsShader != null )
        {
            m_kAllEllipsoidsShader.dispose();
            m_kAllEllipsoidsShader = null;
        }
        if ( m_kEllipseMaterial != null )
        {
            m_kEllipseMaterial.dispose();
            m_kEllipseMaterial = null;
        }
        if ( m_kSphere != null )
        {
            m_kSphere.dispose();
            m_kSphere = null;
        }
        if ( m_kColorEllipse != null )
        {
            m_kColorEllipse.dispose();
            m_kColorEllipse = null;
        }
    }

    /** Hashmap for multiple fiber bundles: */
    private HashMap<Integer,Node>  m_kTracts = null;
    
    /** Hashmap for multiple tube type fiber bundles: */
    private HashMap<Integer,Node>  m_kTubes = null;
    private HashMap<Integer,Integer>  m_kTubeColors = null;

    /** Hashmap for multiple fiber bundles: */
    private HashMap<Integer,ShaderEffect>  m_kShaders = null;

    /** Hashmap for multiple fiber bundles: */
    private HashMap<Integer,Vector<int[]>> m_kEllipsoids = null;

    /** When true display the fiber tracts with ellipsoids instead of lines: */
    private boolean m_bDisplayEllipsoids = false;
    /** When true display the DTI volume with ellipsoids: */
    private boolean m_bDisplayAllEllipsoids = false;
    /** In the display all ellipsoids mode the ellipsoids are displayed every
     * m_iEllipsoidMod steps. */
    private int m_iEllipsoidMod = 10;
    
    /** Hashmap for multiple fiber bundles: */
    private HashMap<Integer,Vector<int[]>> m_kCylinders = null;
    /** When true display the fiber tracts with cylinders instead of lines: */
    private boolean m_bDisplayCylinders = false;
    /** When true display the DTI volume with cylinders: */
    private boolean m_bDisplayAllCylinders = false;
    /** When true display the fiber tracts with cylinders instead of lines: */
    private boolean m_bDisplayTubes = false;
    
    /** Shader for displaying the ellipsoids with the Mipav lights. */
    private MipavLightingEffect m_kAllEllipsoidsShader = null;
    
    private SurfaceLightingEffect m_kLightShader;
    
    private TextureEffect textureEffect;
    
    /** Keeps track of the color assigned the polylines. */
    private HashMap<Integer,ColorRGB> m_kEllipseConstantColor;
    /** Material properties of the ellipsoids. */
    private MaterialState m_kEllipseMaterial;
    /** Material properties of the Tubes. */
    private MaterialState m_kTubesMaterial;
    /** EigenVector values for displaying ellipsoids. */
    private HashMap<Integer,Transformation>  m_kEigenVectors = null;
    /** Ellipsoids is a sphere with a non-uniform scale based on the eigen
     * vectors and values. */
    private TriMesh m_kSphere;
    /** Cylinders is a sphere with a non-uniform scale based on the eigen
     * vectors and values. */
    private TriMesh m_kCylinder;
    /** The DTI volume extents: */
    private int m_iDimX, m_iDimY, m_iDimZ;
    /** Ellispods scale factor, based on the DTI volume: */
    private float m_fScale;
    /** DTI volume data size: */
    private int m_iLen;
    /** Volume-based color for the ellipsoids: */
    private ColorRGB m_kColorEllipse;
    /** maximum number of fiber tracts currently displayed. */
    private int m_iMaxGroups = 0;
    
    /** Color Shader for rendering the tracts. */
    private ShaderEffect m_kVertexColor3Shader;
    
    private int centerIndex;
}
