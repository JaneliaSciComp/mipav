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

package gov.nih.mipav.view.WildMagic.LibFoundation.Approximation;

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibFoundation.NumericalAnalysis.*;

public class ApprGaussPointsFit3f
{

    // Fit points with a Gaussian distribution.  The center is the mean of the
    // points, the axes are the eigenvectors of the covariance matrix, and the
    // extents are the eigenvalues of the covariance matrix and are returned in
    // increasing order.  The quantites are stored in a Box3<Real> just to have a
    // single container.
    public static Box3f GaussPointsFit3f (int iQuantity, final Vector3f[] akPoint)
    {
        Box3f kBox = new Box3f(Vector3f.ZERO,Vector3f.UNIT_X,
                        Vector3f.UNIT_Y,Vector3f.UNIT_Z,1.0f,1.0f,
                        1.0f);

        // compute the mean of the points
        kBox.Center.SetData(akPoint[0]);
        int i;
        for (i = 1; i < iQuantity; i++)
        {
            kBox.Center.addEquals(akPoint[i]);
        }
        float fInvQuantity = (1.0f)/(float)iQuantity;
        kBox.Center.scaleEquals(fInvQuantity);

        // compute the covariance matrix of the points
        float fSumXX = 0.0f, fSumXY = 0.0f, fSumXZ = 0.0f;
        float fSumYY = 0.0f, fSumYZ = 0.0f, fSumZZ = 0.0f;
        for (i = 0; i < iQuantity; i++)
        {
            Vector3f kDiff = akPoint[i].sub(kBox.Center);
            fSumXX += kDiff.X()*kDiff.X();
            fSumXY += kDiff.X()*kDiff.Y();
            fSumXZ += kDiff.X()*kDiff.Z();
            fSumYY += kDiff.Y()*kDiff.Y();
            fSumYZ += kDiff.Y()*kDiff.Z();
            fSumZZ += kDiff.Z()*kDiff.Z();
        }

        fSumXX *= fInvQuantity;
        fSumXY *= fInvQuantity;
        fSumXZ *= fInvQuantity;
        fSumYY *= fInvQuantity;
        fSumYZ *= fInvQuantity;
        fSumZZ *= fInvQuantity;

        // setup the eigensolver
        Eigenf kES = new Eigenf(3);
        kES.SetData(0,0, fSumXX);
        kES.SetData(0,1, fSumXY);
        kES.SetData(0,2, fSumXZ);
        kES.SetData(1,0, fSumXY);
        kES.SetData(1,1, fSumYY);
        kES.SetData(1,2, fSumYZ);
        kES.SetData(2,0, fSumXZ);
        kES.SetData(2,1, fSumYZ);
        kES.SetData(2,2, fSumZZ);
        kES.IncrSortEigenStuff3();

        for (i = 0; i < 3; i++)
        {
            kBox.Extent[i] = kES.GetEigenvalue(i);
            kES.GetEigenvector(i,kBox.Axis[i]);
        }

        return kBox;
    }

}
