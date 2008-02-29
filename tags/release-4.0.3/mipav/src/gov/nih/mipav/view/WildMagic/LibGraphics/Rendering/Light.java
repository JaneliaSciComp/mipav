// Wild Magic Source Code
// David Eberly
// http://www.geometrictools.com
// Copyright (c) 1998-2007
//
// This library is free software; you can redistribute it and/or modify it
// under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or (at
// your option) any later version.  The license is available for reading at
// either of the locations:
//     http://www.gnu.org/copyleft/lgpl.html
//     http://www.geometrictools.com/License/WildMagicLicense.pdf
//
// Version: 4.0.0 (2006/06/28)
//
// Ported to Java by Alexandra Bokinsky, PhD, Geometric Tools, Inc. (July 2007)
//

package gov.nih.mipav.view.WildMagic.LibGraphics.Rendering;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;

/** If the Light class were to have no data, or just ambient color and
 * intensity, you could use a standard class hierarchy:
 *
 *   class Light
 *       [ambient, intensity]
 *   class AmbientLight : public Light
 *       [no additional data]
 *   class DirectionalLight : public Light
 *       [direction, diffuse, specular]
 *   class PointLight : public Light
 *       [position, diffuse, specular, attenuation]
 *   class SpotLight : public PointLight
 *       [cone axis, cone angle, spot exponent]
 *
 * The renderer holds onto lights via the base class Light.  The
 * consequences of a standard class hierarchy are that the renderer must
 * use dynamic casting to determine the type of a light in order to set
 * shader program constants in the Renderer::SetConstantLightFOOBAR calls.
 * This is an expense I wish to avoid.
 *
 * An alternative is to allow Light to store all the data in public scope,
 * but to derive the specific light classes using a protected Light base
 * class.  Thus, Renderer has access to all the data it needs without
 * having to dynamically cast and the derived-class objects have functions
 * to access only the data relevant to them.  Unfortunately, you run into
 * problems with access rights to Object items (such as increment and
 * decrement of reference counts for smart pointers).
 *
 * In the end, I chose to make the Light class a generic class that
 * stores everything needed by the various light types.
 */
public class Light extends GraphicsObject
    implements StreamInterface
{
    /** Light types. */
    public enum LightType
    {
        LT_AMBIENT,
        LT_DIRECTIONAL,
        LT_POINT,
        LT_SPOT,
        LT_QUANTITY;

        private int m_iValue;
        private static int m_iInitValue = 0;
        LightType()
        {
            m_iValue = Init();
        }
        private int Init ()
        {
            return m_iInitValue++;
        }
        public int Value() { return m_iValue; }
    };

    /** Default light constructor, type defaults to ambient light. */
    public Light ()
    {
        init(LightType.LT_AMBIENT);
    }
    
    /** Light constructor, creates a light of the given type.
     * @param eType, type of light to create.
     */
    public Light (LightType eType)
    {
        init(eType);
    }

    /** delete memory */
    public void dispose ()
    {
        if ( Ambient != null )
        {
            Ambient.dispose();
            Ambient = null;
        }
        if ( Diffuse!= null )
        {
            Diffuse.dispose();
            Diffuse = null;
        }
        if ( Specular != null )
        {
            Specular.dispose();
            Specular = null;
        }
        if ( Position != null )
        {
            Position.dispose();
            Position = null;
        }
        if ( DVector != null )
        {
            DVector.dispose();
            DVector = null;
        }
        if ( UVector != null )
        {
            UVector.dispose();
            UVector = null;
        }
        if ( RVector != null )
        {
            RVector.dispose();
            RVector = null;
        }
        super.dispose();
    }
    
    /** Init the light parameters based on the type.
     * @param eType, type of light to create.
     */
    private void init(LightType eType)
    {
        Type = eType;
        Ambient = new ColorRGB(0.0f,0.0f,0.0f);
        Diffuse = new ColorRGB(0.0f,0.0f,0.0f);
        Specular = new ColorRGB(0.0f,0.0f,0.0f);
        Position = new Vector3f(0.0f,0.0f,0.0f);
        DVector = new Vector3f(0.0f,0.0f,-1.0f);
        UVector = new Vector3f(0.0f,1.0f,0.0f);
        RVector = new Vector3f(1.0f,0.0f,0.0f);
        Constant = 1.0f;
        Linear = 0.0f;
        Quadratic = 0.0f;
        Intensity = 1.0f;
        Angle = (float)Math.PI;
        CosAngle = -1.0f;
        SinAngle = 0.0f;
        Exponent = 1.0f;
    }

    /** Type default: LT_AMBIENT */
    public LightType Type;     

    /** The Ambient color of the light. default: ColorRGB(0,0,0) */
    public ColorRGB Ambient;   
    /** The Diffuse color of the light. default: ColorRGB(0,0,0) */
    public ColorRGB Diffuse;
    /** The Specular color of the light. default: ColorRGB(0,0,0) */
    public ColorRGB Specular;

    /** Attenuation is typically specified as a modulator
     *   m = 1/(C + L*d + Q*d*d)
     * where C is the constant coefficient, L is the linear coefficient,
     * Q is the quadratic coefficient, and d is the distance from the light
     * position to the vertex position.  To allow for a linear adjustment of
     * intensity, my choice is to use instead
     *   m = I/(C + L*d + Q*d*d)
     * where I is an "intensity" factor.
     */
    public float Constant;     // default: 1
    public float Linear;       // default: 0
    public float Quadratic;    // default: 0
    public float Intensity;    // default: 1

    /** Parameters for spot lights.  The cone angle must be in radians and
     * should satisfy 0 < Angle <= pi. */
    public float Angle;        // default: pi
    public float CosAngle;     // default: -1
    public float SinAngle;     // default:  0
    public float Exponent;     // default:  1

    /** A helper function that lets you set Angle and have CosAngle and
     * SinAngle computed for you.
     * @param fAngle, set the angle value.
     */
    public void SetAngle (float fAngle)
    {
        assert(0.0f < fAngle && fAngle <= Math.PI);
        Angle = fAngle;
        CosAngle = (float)Math.cos(fAngle);
        SinAngle = (float)Math.sin(fAngle);
    }


    /** Although the standard directional and spot lights need only a direction
     * vector, to allow for new types of derived-class lights that would use
     * a full coordinate frame, Light provides storage for such a frame.  The
     * light frame is always in world coordinates.
     *   default position  P = (0,0,0)
     *   default direction D = (0,0,-1)
     *   default up        U = (0,1,0)
     *   default right     R = (1,0,0)
     * The set {D,U,R} must be a right-handed orthonormal set.  That is, each
     * vector is unit length, the vectors are mutually perpendicular, and
     * R = Cross(D,U).
     */
    public Vector3f Position, DVector, UVector, RVector;

    /** A helper function that lets you set the direction vector and computes
     * the up and right vectors automatically.
     * @param rkDirection, sets the direction vector.
     */
    public void SetDirection (final Vector3f rkDirection)
    {
        DVector = new Vector3f(rkDirection);
        Vector3f.GenerateOrthonormalBasis(UVector,RVector,DVector);
    }

    /** This is for debug mode to allow you to check if the coordinate frame
     * vectors form a right-handed orthonormal set.
     * @return true if valid frame.
     */
    public boolean IsValidFrame ()
    {
        float fTest = DVector.Dot(UVector);
        if (Math.abs(fTest) > Mathf.ZERO_TOLERANCE)
        {
            return false;
        }

        fTest = DVector.Dot(RVector);
        if (Math.abs(fTest) > Mathf.ZERO_TOLERANCE)
        {
            return false;
        }

        fTest = UVector.Dot(RVector);
        if (Math.abs(fTest) > Mathf.ZERO_TOLERANCE)
        {
            return false;
        }
        Vector3f kCross = new Vector3f();
        UVector.Cross(RVector, kCross);
        fTest = DVector.Dot(kCross);
        kCross = null;
        return Math.abs(1.0f - fTest) <= Mathf.ZERO_TOLERANCE;
    }

    /**
     * Loads this object from the input parameter rkStream, using the input
     * Stream.Link to store the IDs of children objects of this object
     * for linking after all objects are loaded from the Stream.
     * @param rkStream, the Stream from which this object is being read.
     * @param pkLink, the Link class for storing the IDs of this object's
     * children objcts.
     */
    public void Load (Stream rkStream, Stream.Link pkLink)
    {
        super.Load(rkStream,pkLink);

        // native data
        rkStream.Read(Position);
        rkStream.Read(DVector);
        rkStream.Read(UVector);
        rkStream.Read(RVector);
        rkStream.Read(Ambient);
        rkStream.Read(Diffuse);
        rkStream.Read(Specular);
        Intensity = rkStream.ReadFloat();
        Constant = rkStream.ReadFloat();
        Linear = rkStream.ReadFloat();
        Quadratic = rkStream.ReadFloat();
        Angle = rkStream.ReadFloat();
        CosAngle = rkStream.ReadFloat();
        SinAngle = rkStream.ReadFloat();
        Exponent = rkStream.ReadFloat();
    }

    /**
     * Write this object and all it's children to the Stream.
     * @param rkStream, the Stream where the child objects are stored.
     */
    public void Save (Stream rkStream)
    {
        super.Save(rkStream);

        // native data
        rkStream.Write(Position);
        rkStream.Write(DVector);
        rkStream.Write(UVector);
        rkStream.Write(RVector);
        rkStream.Write(Ambient);
        rkStream.Write(Diffuse);
        rkStream.Write(Specular);
        rkStream.Write(Intensity);
        rkStream.Write(Constant);
        rkStream.Write(Linear);
        rkStream.Write(Quadratic);
        rkStream.Write(Angle);
        rkStream.Write(CosAngle);
        rkStream.Write(SinAngle);
        rkStream.Write(Exponent);
    }

    /**
     * Returns the size of this object and it's children on disk for the
     * current StreamVersion parameter.
     * @param rkVersion, the current version of the Stream file being created.
     * @return the size of this object on disk.
     */
    public int GetDiskUsed (final StreamVersion rkVersion)
    {
        return super.GetDiskUsed(rkVersion) +
            3 * Stream.SIZEOF_FLOAT + //sizeof(Position) +
            3 * Stream.SIZEOF_FLOAT + //sizeof(DVector) +
            3 * Stream.SIZEOF_FLOAT + //sizeof(UVector) +
            3 * Stream.SIZEOF_FLOAT + //sizeof(RVector) +
            3 * Stream.SIZEOF_FLOAT + //sizeof(Ambient) +
            3 * Stream.SIZEOF_FLOAT + //sizeof(Diffuse) +
            3 * Stream.SIZEOF_FLOAT + //sizeof(Specular) +
            Stream.SIZEOF_FLOAT + //sizeof(Intensity) +
            Stream.SIZEOF_FLOAT + //sizeof(Constant) +
            Stream.SIZEOF_FLOAT + //sizeof(Linear) +
            Stream.SIZEOF_FLOAT + //sizeof(Quadratic) +
            Stream.SIZEOF_FLOAT + //sizeof(Angle) +
            Stream.SIZEOF_FLOAT + //sizeof(CosAngle) +
            Stream.SIZEOF_FLOAT + //sizeof(SinAngle) +
            Stream.SIZEOF_FLOAT; //sizeof(Exponent);
    }

    /**
     * Write this object into a StringTree for the scene-graph visualization.
     * @param acTitle, the header for this object in the StringTree.
     * @return StringTree containing a String-based representation of this
     * object and it's children.
     */
    public StringTree SaveStrings (final String acTitle)
    {
        StringTree pkTree = new StringTree();
        // strings
        pkTree.Append(StringTree.Format("Light",GetName()));
        pkTree.Append(super.SaveStrings(null));
        pkTree.Append(StringTree.Format("position =",Position));
        pkTree.Append(StringTree.Format("d vector =",DVector));
        pkTree.Append(StringTree.Format("u vector =",UVector));
        pkTree.Append(StringTree.Format("r vector =",RVector));
        pkTree.Append(StringTree.Format("ambient =",Ambient));
        pkTree.Append(StringTree.Format("diffuse =",Diffuse));
        pkTree.Append(StringTree.Format("specular =",Specular));
        pkTree.Append(StringTree.Format("intensity =",Intensity));
        pkTree.Append(StringTree.Format("attn constant =",Constant));
        pkTree.Append(StringTree.Format("attn linear =",Linear));
        pkTree.Append(StringTree.Format("attn quadratic =",Quadratic));
        pkTree.Append(StringTree.Format("angle =",Angle));
        pkTree.Append(StringTree.Format("exponent =",Exponent));
        return pkTree;
    }
}
