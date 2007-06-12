// Geometric Tools, Inc.
// http://www.geometrictools.com
// Copyright (c) 1998-2006.  All Rights Reserved
//
// The Wild Magic Version 4 Restricted Libraries source code is supplied
// under the terms of the license agreement
//     http://www.geometrictools.com/License/Wm4RestrictedLicense.pdf
// and may not be copied or disclosed except in accordance with the terms
// of that agreement.
//
// Version: 4.0.0 (2006/06/28)

package gov.nih.mipav.view.WildMagic.LibGraphics.Shaders;

import java.util.HashMap;

public class RendererConstant
{

    // For a lookup of the renderer constant type from its string name.
    private static HashMap<String,Type> ms_pkTypeMap = new HashMap<String,Type>();

    public static enum Type
    {
        W_MATRIX ("WMatrix"),                          // 4x4 model-to-world matrix
        V_MATRIX ("VMatrix"),                          // 4x4 world-to-view matrix
        P_MATRIX ("PMatrix"),                          // 4x4 view-to-clip matrix
        WV_MATRIX ("WVMatrix"),                         // 4x4 model-to-view matrix
        VP_MATRIX ("VPMatrix"),                         // 4x4 world-to-clip matrix
        WVP_MATRIX ("WVPMatrix"),                        // 4x4 model-to-clip matrix
        W_MATRIX_TRANSPOSE ("WMatrixT"),                // 4x4 trans model-to-world
        V_MATRIX_TRANSPOSE ("VMatrixT"),                // 4x4 trans world-to-view
        P_MATRIX_TRANSPOSE ("PMatrixT"),                // 4x4 trans view-to-clip
        WV_MATRIX_TRANSPOSE ("WVMatrixT"),               // 4x4 trans model-to-view
        VP_MATRIX_TRANSPOSE ("VPMatrixT"),               // 4x4 trans world-to-clip
        WVP_MATRIX_TRANSPOSE ("WVPMatrixT"),              // 4x4 trans model-to-clip
        W_MATRIX_INVERSE ("WMatrixI"),                  // 4x4 inv model-to-world
        V_MATRIX_INVERSE ("VMatrixI"),                  // 4x4 inv world-to-view
        P_MATRIX_INVERSE ("PMatrixI"),                  // 4x4 inv view-to-clip
        WV_MATRIX_INVERSE ("WVMatrixI"),                 // 4x4 inv model-to-view
        VP_MATRIX_INVERSE ("VPMatrixI"),                 // 4x4 inv world-to-clip
        WVP_MATRIX_INVERSE ("WVPMatrixI"),                // 4x4 inv model-to-clip
        W_MATRIX_INVERSE_TRANSPOSE ("WMatrixIT"),        // 4x4 inv-trans model-to-world
        V_MATRIX_INVERSE_TRANSPOSE ("VMatrixIT"),        // 4x4 inv-trans world-to-view
        P_MATRIX_INVERSE_TRANSPOSE ("PMatrixIT"),        // 4x4 inv-trans view-to-clip
        WV_MATRIX_INVERSE_TRANSPOSE ("WVMatrixIT"),       // 4x4 inv-trans model-to-view
        VP_MATRIX_INVERSE_TRANSPOSE ("VPMatrixIT"),       // 4x4 inv-trans world-to-clip
        WVP_MATRIX_INVERSE_TRANSPOSE ("WVPMatrixIT"),      // 4x4 inv-trans model-to-clip

        MATERIAL_EMISSIVE ("MaterialEmissive"),                 // (r,g,b)
        MATERIAL_AMBIENT ("MaterialAmbient"),                  // (r,g,b)
        MATERIAL_DIFFUSE ("MaterialDiffuse"),                  // (r,g,b;alpha)
        MATERIAL_SPECULAR ("MaterialSpecular"),                 // (r,g,b;shininess)

        CAMERA_MODEL_POSITION ("CameraModelPosition"),             // (x,y,z,1)
        CAMERA_MODEL_DIRECTION ("CameraModelDirection"),            // (x,y,z,0)
        CAMERA_MODEL_UP ("CameraModelUp"),                   // (x,y,z,0)
        CAMERA_MODEL_RIGHT ("CameraModelRight"),                // (x,y,z,0)

        CAMERA_WORLD_POSITION ("CameraWorldPosition"),             // (x,y,z,1)
        CAMERA_WORLD_DIRECTION ("CameraWorldDirection"),            // (x,y,z,0)
        CAMERA_WORLD_UP ("CameraWorldUp"),                   // (x,y,z,0)
        CAMERA_WORLD_RIGHT ("CameraWorldRight"),                // (x,y,z,0)

        PROJECTOR_MODEL_POSITION ("ProjectorModelPosition"),          // (x,y,z,1)
        PROJECTOR_MODEL_DIRECTION ("ProjectorModelDirection"),         // (x,y,z,0)
        PROJECTOR_MODEL_UP ("ProjectorModelUp"),                // (x,y,z,0)
        PROJECTOR_MODEL_RIGHT ("ProjectorModelRight"),             // (x,y,z,0)

        PROJECTOR_WORLD_POSITION ("ProjectorWorldPosition"),          // (x,y,z,1)
        PROJECTOR_WORLD_DIRECTION ("ProjectorWorldDirection"),         // (x,y,z,0)
        PROJECTOR_WORLD_UP ("ProjectorWorldUp"),                // (x,y,z,0)
        PROJECTOR_WORLD_RIGHT ("ProjectorWorldRight"),             // (x,y,z,0)

        PROJECTOR_MATRIX ("ProjectorMatrix"),                  // 4x4 world-to-clip matrix

        LIGHT0_MODEL_POSITION ("Light0ModelPosition"),             // (x,y,z,1)
        LIGHT0_MODEL_DIRECTION ("Light0ModelDirection"),            // (x,y,z,0)
        LIGHT0_WORLD_POSITION ("Light0WorldPosition"),             // (x,y,z,1)
        LIGHT0_WORLD_DIRECTION ("Light0WorldDirection"),            // (x,y,z,0)
        LIGHT0_AMBIENT ("Light0Ambient"),                    // (r,g,b,a)
        LIGHT0_DIFFUSE ("Light0Diffuse"),                    // (r,g,b,a)
        LIGHT0_SPECULAR ("Light0Specular"),                   // (r,g,b,a)
        LIGHT0_SPOTCUTOFF ("Light0SpotCutoff"),                 // (angle,cos,sin,exponent)
        LIGHT0_ATTENUATION ("Light0Attenuation"),                // (const,lin,quad,intensity)

        LIGHT1_MODEL_POSITION ("Light1ModelPosition"),             // (x,y,z,1)
        LIGHT1_MODEL_DIRECTION ("Light1ModelDirection"),            // (x,y,z,0)
        LIGHT1_WORLD_POSITION ("Light1WorldPosition"),             // (x,y,z,1)
        LIGHT1_WORLD_DIRECTION ("Light1WorldDirection"),            // (x,y,z,0)
        LIGHT1_AMBIENT ("Light1Ambient"),                    // (r,g,b,a)
        LIGHT1_DIFFUSE ("Light1Diffuse"),                    // (r,g,b,a)
        LIGHT1_SPECULAR ("Light1Specular"),                   // (r,g,b,a)
        LIGHT1_SPOTCUTOFF ("Light1SpotCutoff"),                 // (angle,cos,sin,exponent)
        LIGHT1_ATTENUATION ("Light1Attenuation"),                // (const,lin,quad,intensity)

        LIGHT2_MODEL_POSITION ("Light2ModelPosition"),             // (x,y,z,1)
        LIGHT2_MODEL_DIRECTION ("Light2ModelDirection"),            // (x,y,z,0)
        LIGHT2_WORLD_POSITION ("Light2WorldPosition"),             // (x,y,z,1)
        LIGHT2_WORLD_DIRECTION ("Light2WorldDirection"),            // (x,y,z,0)
        LIGHT2_AMBIENT ("Light2Ambient"),                    // (r,g,b,a)
        LIGHT2_DIFFUSE ("Light2Diffuse"),                    // (r,g,b,a)
        LIGHT2_SPECULAR ("Light2Specular"),                   // (r,g,b,a)
        LIGHT2_SPOTCUTOFF ("Light2SpotCutoff"),                 // (angle,cos,sin,exponent)
        LIGHT2_ATTENUATION ("Light2Attenuation"),                // (const,lin,quad,intensity)

        LIGHT3_MODEL_POSITION ("Light3ModelPosition"),             // (x,y,z,1)
        LIGHT3_MODEL_DIRECTION ("Light3ModelDirection"),            // (x,y,z,0)
        LIGHT3_WORLD_POSITION ("Light3WorldPosition"),             // (x,y,z,1)
        LIGHT3_WORLD_DIRECTION ("Light3WorldDirection"),            // (x,y,z,0)
        LIGHT3_AMBIENT ("Light3Ambient"),                    // (r,g,b,a)
        LIGHT3_DIFFUSE ("Light3Diffuse"),                    // (r,g,b,a)
        LIGHT3_SPECULAR ("Light3Specular"),                   // (r,g,b,a)
        LIGHT3_SPOTCUTOFF ("Light3SpotCutoff"),                 // (angle,cos,sin,exponent)
        LIGHT3_ATTENUATION ("Light3Attenuation"),                // (const,lin,quad,intensity)

        LIGHT4_MODEL_POSITION ("Light4ModelPosition"),             // (x,y,z,1)
        LIGHT4_MODEL_DIRECTION ("Light4ModelDirection"),            // (x,y,z,0)
        LIGHT4_WORLD_POSITION ("Light4WorldPosition"),             // (x,y,z,1)
        LIGHT4_WORLD_DIRECTION ("Light4WorldDirection"),            // (x,y,z,0)
        LIGHT4_AMBIENT ("Light4Ambient"),                    // (r,g,b,a)
        LIGHT4_DIFFUSE ("Light4Diffuse"),                    // (r,g,b,a)
        LIGHT4_SPECULAR ("Light4Specular"),                   // (r,g,b,a)
        LIGHT4_SPOTCUTOFF ("Light4SpotCutoff"),                 // (angle,cos,sin,exponent)
        LIGHT4_ATTENUATION ("Light4Attenuation"),                // (const,lin,quad,intensity)

        LIGHT5_MODEL_POSITION ("Light5ModelPosition"),             // (x,y,z,1)
        LIGHT5_MODEL_DIRECTION ("Light5ModelDirection"),            // (x,y,z,0)
        LIGHT5_WORLD_POSITION ("Light5WorldPosition"),             // (x,y,z,1)
        LIGHT5_WORLD_DIRECTION ("Light5WorldDirection"),            // (x,y,z,0)
        LIGHT5_AMBIENT ("Light5Ambient"),                    // (r,g,b,a)
        LIGHT5_DIFFUSE ("Light5Diffuse"),                    // (r,g,b,a)
        LIGHT5_SPECULAR ("Light5Specular"),                   // (r,g,b,a)
        LIGHT5_SPOTCUTOFF ("Light5SpotCutoff"),                 // (angle,cos,sin,exponent)
        LIGHT5_ATTENUATION ("Light5Attenuation"),                // (const,lin,quad,intensity)

        LIGHT6_MODEL_POSITION ("Light6ModelPosition"),             // (x,y,z,1)
        LIGHT6_MODEL_DIRECTION ("Light6ModelDirection"),            // (x,y,z,0)
        LIGHT6_WORLD_POSITION ("Light6WorldPosition"),             // (x,y,z,1)
        LIGHT6_WORLD_DIRECTION ("Light6WorldDirection"),            // (x,y,z,0)
        LIGHT6_AMBIENT ("Light6Ambient"),                    // (r,g,b,a)
        LIGHT6_DIFFUSE ("Light6Diffuse"),                    // (r,g,b,a)
        LIGHT6_SPECULAR ("Light6Specular"),                   // (r,g,b,a)
        LIGHT6_SPOTCUTOFF ("Light6SpotCutoff"),                 // (angle,cos,sin,exponent)
        LIGHT6_ATTENUATION ("Light6Attenuation"),                // (const,lin,quad,intensity)

        LIGHT7_MODEL_POSITION ("Light7ModelPosition"),             // (x,y,z,1)
        LIGHT7_MODEL_DIRECTION ("Light7ModelDirection"),            // (x,y,z,0)
        LIGHT7_WORLD_POSITION ("Light7WorldPosition"),             // (x,y,z,1)
        LIGHT7_WORLD_DIRECTION ("Light7WorldDirection"),            // (x,y,z,0)
        LIGHT7_AMBIENT ("Light7Ambient"),                    // (r,g,b,a)
        LIGHT7_DIFFUSE ("Light7Diffuse"),                    // (r,g,b,a)
        LIGHT7_SPECULAR ("Light7Specular"),                   // (r,g,b,a)
        LIGHT7_SPOTCUTOFF ("Light7SpotCutoff"),                 // (angle,cos,sin,exponent)
        LIGHT7_ATTENUATION ("Light7Attenuation"),                // (const,lin,quad,intensity)
        
        MAX_TYPES ("MaxTypes");

        private String m_kName;
        private int m_iValue;
        private static int m_iInitValue = 0;
        Type( String kName )
        {
            m_kName = kName;
            m_iValue = Init();
            ms_pkTypeMap.put( m_kName, this );
        }
        private int Init ()
        {
            return m_iInitValue++;
        }
        public String Name() { return m_kName; }
        public int Value() { return m_iValue; }
    };
    
    private static Type m_eTypeStatic = Type.MAX_TYPES;

    // Construction and destruction.  The base register must be nonnegative.
    // The register quantity is between 1 and 4.  Each register represents
    // four floating-point values.
    public RendererConstant (Type eType, int iBaseRegister, int iRegisterQuantity)
    {
        assert(iBaseRegister >= 0);
        assert(1 <= iRegisterQuantity && iRegisterQuantity <= 4);

        m_eType = eType;
        m_iBaseRegister = iBaseRegister;
        m_iRegisterQuantity = iRegisterQuantity;
    }

    // Member access.  The renderer will use these to set the registers with
    // the appropriate values.
    public Type GetType ()
    {
        return m_eType;
    }

    public int GetBaseRegister ()
    {
        return m_iBaseRegister;
    }

    public int GetRegisterQuantity ()
    {
        return m_iRegisterQuantity;
    }

    public float[] GetData ()
    {
        return m_afData;
    }

    // Mappings between enums and strings.
    public static final String GetName (Type eType)
    {
        return eType.Name();
    }

    public static Type GetType (String rkName)
    {
        Type kType = ms_pkTypeMap.get(rkName);
        if ( kType != null )
        {
            return kType;
        }
        return Type.MAX_TYPES;
    }

    private Type m_eType;
    private int m_iBaseRegister;
    private int m_iRegisterQuantity;  // maximum of 4
    private float[] m_afData = new float[16];       // maximum storage, avoid dynamic allocation
}
