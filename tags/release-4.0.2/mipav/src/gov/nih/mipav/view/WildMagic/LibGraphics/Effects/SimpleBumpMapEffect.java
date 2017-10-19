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

package gov.nih.mipav.view.WildMagic.LibGraphics.Effects;

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Shaders.*;

public class SimpleBumpMapEffect extends ShaderEffect
    implements StreamInterface
{
    /**
     * Creates a new BumpMapEffect.
     * @param acBaseName, the base texture name
     * @param acNormalName, the normal map texture name
     * @param rkLightDirection, the current light direction vector.
     */
    public SimpleBumpMapEffect ( final String acBaseName, final String acNormalName,
                                 final Vector3f rkLightDirection)
    {
        super(1);
        m_kLightDirection = new Vector3f(rkLightDirection);
        m_kVShader.set(0, new VertexShader("SimpleBumpMap"));
        m_kPShader.set(0, new PixelShader("SimpleBumpMap"));

        m_kPShader.get(0).SetTextureQuantity(2);
        m_kPShader.get(0).SetImageName(0,acBaseName);
        m_kPShader.get(0).SetImageName(1,acNormalName);
    }

    /** Delete data members: */
    public void dispose ()
    {
        if ( m_kLightDirection != null )
        {
            m_kLightDirection.dispose();
            m_kLightDirection = null;
        }
        super.dispose();
    }

    /** Sets the light direction
     * @param rkLightDirection, the light direction vector
     */
    public void SetLightDirection (final Vector3f rkLightDirection)
    {
        m_kLightDirection = rkLightDirection;
    }

    /** Returns the light direction vector
     * @return, the light direction vector
     */
    public Vector3f GetLightDirection ()
    {
        return m_kLightDirection;
    }

    /**
     * Computes the LightVectors for the Triangle mesh 
     * @param pkMesh
     */
    public void ComputeLightVectors (Triangles pkMesh)
    {
        // The tangent-space coordinates for the light direction vector at each
        // vertex is stored in the color0 channel.  The computations use the
        // vertex normals and the texture coordinates for the base mesh, which
        // are stored in the tcoord0 channel.
        assert((pkMesh != null) && (pkMesh.VBuffer != null) && (pkMesh.IBuffer != null));
        assert(pkMesh.VBuffer.GetAttributes().GetPChannels() == 3);
        assert(pkMesh.VBuffer.GetAttributes().GetNChannels() == 3);
        assert(pkMesh.VBuffer.GetAttributes().GetCChannels(0) == 3);
        assert(pkMesh.VBuffer.GetAttributes().GetTChannels(0) == 2);

        // The light direction D is in world-space coordinates.  Negate it,
        // transform it to model-space coordinates, and then normalize it.  The
        // world-space direction is unit-length, but the geometric primitive
        // might have non-unit scaling in its model-to-world transformation, in
        // which case the normalization is necessary.
        Vector3f kModelLightDirection = new Vector3f(m_kLightDirection);
        kModelLightDirection.negEquals();
        kModelLightDirection = pkMesh.World.InvertVector(kModelLightDirection);

        // Set the light vectors to (0,0,0) as a flag that the quantity has not
        // yet been computed.  The probability that a light vector is actually
        // (0,0,0) should be small, so the flag system should save computation
        // time overall.
        VertexBuffer pkVB = pkMesh.VBuffer;
        int iVQuantity = pkMesh.VBuffer.GetVertexQuantity();
        int i;
        for (i = 0; i < iVQuantity; i++)
        {
            pkVB.SetColor3(0,i, ColorRGB.BLACK);
        }

        int iTQuantity = pkMesh.GetTriangleQuantity();
        
        Vector3f[] apkV = new Vector3f[3];
        Vector3f[] apkN = new Vector3f[3];
        ColorRGB[] apkC = new ColorRGB[3];
        Vector2f[] apkST = new Vector2f[3];
        for ( i = 0; i < 3; i++ )
        {
            apkV[i] = new Vector3f();
            apkN[i] = new Vector3f();
            apkC[i] = new ColorRGB();
            apkST[i] = new Vector2f();
        }
        
        Vector3f kBitangent = new Vector3f();
        for (int iT = 0; iT < iTQuantity; iT++)
        {
            // Get the triangle vertices and attributes.
            int[] aiVerts = new int[3];
            int iV0, iV1, iV2;
            if (!pkMesh.GetTriangle(iT,aiVerts))
            {
                continue;
            }
            iV0 = aiVerts[0];
            iV1 = aiVerts[1];
            iV2 = aiVerts[2];
            pkVB.GetPosition3(iV0, apkV[0]);
            pkVB.GetPosition3(iV1, apkV[1]);
            pkVB.GetPosition3(iV2, apkV[2]);

            pkVB.GetNormal3(iV0, apkN[0]);
            pkVB.GetNormal3(iV1, apkN[1]);
            pkVB.GetNormal3(iV2, apkN[2]);

            pkVB.GetColor3(0,iV0,apkC[0]);
            pkVB.GetColor3(0,iV1,apkC[1]);
            pkVB.GetColor3(0,iV2,apkC[2]);

            pkVB.GetTCoord2(0,iV0, apkST[0]);
            pkVB.GetTCoord2(0,iV1, apkST[1]);
            pkVB.GetTCoord2(0,iV2, apkST[2]);

            for (i = 0; i < 3; i++)
            {
                ColorRGB rkColor = apkC[i];
                if ( (rkColor.R() != ColorRGB.BLACK.R()) ||
                     (rkColor.G() != ColorRGB.BLACK.G()) ||
                     (rkColor.B() != ColorRGB.BLACK.B())   )
                {
                    continue;
                }

                int iP = (i == 0) ? 2 : i - 1;
                int iN = (i + 1) % 3;

                Vector3f kTangent = new Vector3f();
                if (!ComputeTangent(apkV[i],apkST[i],apkV[iN],apkST[iN],
                                    apkV[iP],apkST[iP],kTangent))
                {
                    // The texture coordinate mapping is not properly defined for
                    // this.  Just say that the tangent space light vector points
                    // in the same direction as the surface normal.
                    rkColor.R( apkN[i].X() );
                    rkColor.G( apkN[i].Y() );
                    rkColor.B( apkN[i].Z() );

                    if ( i == 0 )
                    {
                        pkVB.SetColor3(0,iV0, rkColor);
                    }
                    else if ( i == 1 )
                    {
                        pkVB.SetColor3(0,iV1, rkColor);
                    }
                    else
                    {
                        pkVB.SetColor3(0,iV2, rkColor);
                    }
                    continue;
                }

                // Project T into the tangent plane by projecting out the surface
                // normal N, and then make it unit length.
                Vector3f kScale = new Vector3f(apkN[i]);
                kScale.scaleEquals(apkN[i].Dot(kTangent));
                kTangent.subEquals( kScale );
                kScale = null;
                kTangent.Normalize();

                // Compute the bitangent B, another tangent perpendicular to T.
                apkN[i].UnitCross(kTangent, kBitangent);

                // The set {T,B,N} is a right-handed orthonormal set.  The
                // negated light direction U = -D is represented in this
                // coordinate system as
                //   U = Dot(U,T)*T + Dot(U,B)*B + Dot(U,N)*N
                float fDotUT = kModelLightDirection.Dot(kTangent);
                float fDotUB = kModelLightDirection.Dot(kBitangent);
                float fDotUN = kModelLightDirection.Dot(apkN[i]);

                // Transform the light vector into [0,1]^3 to make it a valid
                // ColorRGB object.
                rkColor.R( 0.5f*(fDotUT + 1.0f) );
                rkColor.G( 0.5f*(fDotUB + 1.0f) );
                rkColor.B( 0.5f*(fDotUN + 1.0f) );
                if ( i == 0 )
                {
                    pkVB.SetColor3(0,iV0, rkColor);
                }
                else if ( i == 1 )
                {
                    pkVB.SetColor3(0,iV1, rkColor);
                }
                else
                {
                    pkVB.SetColor3(0,iV2, rkColor);
                }
            }
        }
        kBitangent = null;
        for ( i = 0; i < 3; i++ )
        {
            apkV[i] = null;
            apkN[i] = null;
            apkC[i] = null;
            apkST[i] = null;
        }
    }

    /** Compute a tangent at the vertex P0.  The triangle is counterclockwise
     * ordered, <P0,P1,P2>.
     * @param rkPos0 triangle Position 0
     * @param rkTCoord0 triangle Texture Coordinate 0
     * @param rkPos1 triangle Position 1
     * @param rkTCoord1 triangle Texture Coordinate 1
     * @param rkPos2 triangle Position 2
     * @param rkTCoord2 triangle Texture Coordinate 2
     * @param rkTangent the returned tangent vector
     * @return true on sucess, false on failure.
     */
    protected boolean ComputeTangent (
                                      final Vector3f rkPos0, final Vector2f rkTCoord0,
                                      final Vector3f rkPos1, final Vector2f rkTCoord1,
                                      final Vector3f rkPos2, final Vector2f rkTCoord2,
                                      Vector3f rkTangent)
    {
        // Compute the change in positions at the vertex P0.
        Vector3f kDP1 = new Vector3f();
        rkPos1.sub( rkPos0, kDP1 );
        Vector3f kDP2 = new Vector3f();
        rkPos2.sub( rkPos0, kDP2 );

        if (Math.abs(kDP1.Length()) < Mathf.ZERO_TOLERANCE
            ||  Math.abs(kDP2.Length()) < Mathf.ZERO_TOLERANCE)
        {        
            kDP1 = null;
            kDP2 = null;
            // The triangle is very small, call it degenerate.
            return false;
        }
        
        // Compute the change in texture coordinates at the vertex P0 in the
        // direction of edge P1-P0.
        float fDU1 = rkTCoord1.X() - rkTCoord0.X();
        float fDV1 = rkTCoord1.Y() - rkTCoord0.Y();
        if (Math.abs(fDV1) < Mathf.ZERO_TOLERANCE)
        {
            // The triangle effectively has no variation in the v texture
            // coordinate.
            if (Math.abs(fDU1) < Mathf.ZERO_TOLERANCE)
            {        
                kDP1 = null;
                kDP2 = null;
                // The triangle effectively has no variation in the u coordinate.
                // Since the texture coordinates do not vary on this triangle,
                // treat it as a degenerate parametric surface.
                return false;
            }
            
            // The variation is effectively all in u, so set the tangent vector
            // to be T = dP/du.
            rkTangent.SetData(kDP1);
            rkTangent.divEquals(fDU1);        
            kDP1 = null;
            kDP2 = null;
            return true;
        }

        // Compute the change in texture coordinates at the vertex P0 in the
        // direction of edge P2-P0.
        float fDU2 = rkTCoord2.X() - rkTCoord0.X();
        float fDV2 = rkTCoord2.Y() - rkTCoord0.Y();
        float fDet = fDV1*fDU2 - fDV2*fDU1;
        if (Math.abs(fDet) < Mathf.ZERO_TOLERANCE)
        {        
            kDP1 = null;
            kDP2 = null;
            // The triangle vertices are collinear in parameter space, so treat
            // this as a degenerate parametric surface.
            return false;
        }
        
        // The triangle vertices are not collinear in parameter space, so choose
        // the tangent to be dP/du = (dv1*dP2-dv2*dP1)/(dv1*du2-dv2*du1)
        Vector3f kTemp = new Vector3f(kDP2);
        kTemp.scaleEquals(fDV1);
        Vector3f kScale = new Vector3f(kDP1);
        kScale.scaleEquals(fDV2);
        kTemp.subEquals(kScale);
        kTemp.scale((float)(1.0/fDet), rkTangent);
        kScale = null;
        kTemp = null;
        return true;
    }

    /** The light direction vector for the bump map effect. */
    protected Vector3f m_kLightDirection;

    /** Streaming constructor */
    public SimpleBumpMapEffect ()
    {
        m_kLightDirection = new Vector3f();
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
        rkStream.Read(m_kLightDirection);
    }

    /**
     * Copies this objects children objects from the input Stream's HashTable,
     * based on the LinkID of the child stored in the pkLink paramter.
     * @param rkStream, the Stream where the child objects are stored.
     * @param pkLink, the Link class from which the child object IDs are read.
     */
    public void Link (Stream rkStream, Stream.Link pkLink)
    {
        super.Link(rkStream,pkLink);
    }

    /**
     * Registers this object with the input Stream parameter. All objects
     * streamed to disk are registered with the Stream so that a unique list
     * of objects is maintained.
     * @param rkStream, the Stream where the child objects are stored.
     * @return true if this object is registered, false if the object has
     * already been registered.
     */
    public boolean Register (Stream rkStream)
    {
        return super.Register(rkStream);
    }

    /**
     * Write this object and all it's children to the Stream.
     * @param rkStream, the Stream where the child objects are stored.
     */
    public void Save (Stream rkStream)
    {
        super.Save(rkStream);

        // native data
        rkStream.Write(m_kLightDirection);
    }

    /**
     * Returns the size of this object and it's children on disk for the
     * current StreamVersion parameter.
     * @param rkVersion, the current version of the Stream file being created.
     * @return the size of this object on disk.
     */
    public int GetDiskUsed (StreamVersion rkVersion)
    {
        return super.GetDiskUsed(rkVersion) +
            3 * Stream.SIZEOF_FLOAT; //sizeof(m_kLightDirection);
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
        pkTree.Append(StringTree.Format("SimpleBumpMapEffect",GetName()));
        pkTree.Append(super.SaveStrings(null));
        pkTree.Append(StringTree.Format("light direction =",m_kLightDirection));
        return pkTree;
    }
}