package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.io.*;
import java.util.*;
import javax.vecmath.*;

/**
 * Active Contour class optimized for Cell Tacking, based on the following paper:
 *
 * "Active Contours for Cell Tracking", by Nilanjan Ray and Scott
 * T. Acton. 5th IEEE Southwest Symposium on Image Analysis and Interpretation
 * (SSIAI'02), 2002.
 *
 * Most funcionality is inherited from AlgorithmAGVF, the runSnake function is
 * optimized with the following constraints for cell-tracking:
 *
 * 1). Constrained Gradient Vector Flow: using a Dirichlet boundray contition,
 *     the gradient inside the initial user-defined snake is set to a
 *     user-defined velocity vector.
 *
 * 2). Shape-size constraint: the shape and size of the snake is constrained
 *     to be circular with a user-defined cell radius.
 *
 * 3). Implicit Resampling: the points along the snake curve are minimized
 *     such that they maintain an equal distance between points.
 *
 * The graident vector flow is calculated for each frame in the image
 * sequence. Once the GVF is calculated, the points inside the initial snake
 * (either the user-defined snake for frame 0, or the snake for the previous
 * frame) are set to the velocity vector. runSnake then minimizes the
 * shape-size and resampling constraints for the new GVF, based on the
 * user-defined constraint contributions (see the Lambda member variable). The
 * output snake is used as the initial guess for the next frame in the image
 * sequence.
 * 
 * @see  AlgorithmAGVF
 * @see  GenerateGaussian
 */
public class AlgorithmCellTrackingAGVF extends AlgorithmAGVF {

    //~ Instance fields ------------------------------------------------------------------------------------------------
    /** Constraint factor for the cell shape: */
    private double m_dLambda1 = 1;
    /** Constraint factor for the cell size: */
    private double m_dLambda2 = 1;
    /** Constraint factor for sampling: */
    private double m_dLambda3 = 1;
    /** expected cell radius: */
    private double m_dK = 110;
    /** Previous snake results (X-position): */    
    private double[] m_adX = null;
    /** Previous snake results (Y-position): */    
    private double[] m_adY = null;
    /** Velocity vector (either the initial user-defined guess, or the
     * calculated velocity (new snake center - previous snake center): */
    private Vector2d m_kVelocity = null;
    /** boolean when true, use the velocit vector: */
    private boolean m_bUseVelocity = true;
    /** Previous snake polygon: */
    private Polygon m_kPreviousSnake = null;
    /** counter: */
    private int m_iCount = 0;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmCellTrackingAGVF object.
     *
     * @param  resultImage         image of GVF field magnitude
     * @param  srcImg              2D or 3D source image
     * @param  sigmas              describe the scale of the gaussian in each dimension
     * @param  gvfIterations       iterations in calculating GVF field
     * @param  boundaryIterations  iterations in calculating boundary
     * @param  k                   GVF constant
     * @param  smoothness          DOCUMENT ME!
     * @param  srcVOI              VOI that is to be evolved
     * @param  do25D               only applies to 3D, if true do slice by slice
     * @param  radiusConstraint    The user-defined cell radius
     * @param  shapeConstraint     The contribution of the shape constraint in the minimization
     * @param  sizeConstraint      The contribution of the size constraint in the minimization
     * @param  resamplingConstraint The contribution of the resampling constraint in the minimization
     * @param  fDx                 The initial cell velocity (x-component)
     * @param  fDy                 The initial cell velocity (y-component)
     */
    public AlgorithmCellTrackingAGVF(ModelImage resultImage, ModelImage srcImg,
                                     float[] sigmas, int gvfIterations,
                                     int boundaryIterations, float k,
                                     float smoothness, VOI srcVOI, boolean do25D,
                                     float radiusConstraint,
                                     float shapeConstraint, float sizeConstraint,
                                     float resamplingConstraint,
                                     float fDx, float fDy  ) {
        super(resultImage, srcImg, sigmas,
              gvfIterations, boundaryIterations, k,
              smoothness, srcVOI, do25D);
        /* set the constraint variables: */
        m_dLambda1 = shapeConstraint;
        m_dLambda2 = sizeConstraint;
        m_dLambda3 = resamplingConstraint;
        m_dK = radiusConstraint;
        m_kVelocity = new Vector2d( fDx, fDy );
        if ( (fDx == 0) && (fDy == 0) )
        {
            m_bUseVelocity = false;
        }
        else
        {
            m_kVelocity.normalize();
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize()
    {
        m_adX = null;
        m_adY = null;
        m_kVelocity = null;
        m_kPreviousSnake.reset();
        m_kPreviousSnake = null;
        super.finalize();
    }

    /**
     * Actual function that evolves the boundary.
     *
     * @param  xPoints    x coordinates that describe the contour
     * @param  yPoints    y coordinates that describe the contour
     * @param  u          x component of the GVF
     * @param  v          y component of the GVF
     * @param  resultGon  resultant polygon
     */
    protected void runSnake(float[] xPoints, float[] yPoints,
                            float[] u, float[] v,
                            Polygon resultGon)
    {
        if ( (m_adX != null) && (m_adY != null) )
        {
            System.err.println( "Previous SNAKE" + m_iCount++ );
        }
        else
        {
            System.err.println( "First SNAKE" );
            m_kPreviousSnake = new Polygon();
        }
        /* Get the number of points in the snake, iNPoints: */
        int iNPoints = xPoints.length - 2;
        /* Create matrix-representations (Nx1) for the snake x-position,
         * y-position, x-component of the GVF, y-component of the GVF, Cosine
         * and Sine functions of the number of points: */
        Matrix kX = new Matrix( iNPoints, 1 );
        Matrix kY = new Matrix( iNPoints, 1 );
        Matrix kFx = new Matrix( iNPoints, 1 );
        Matrix kFy = new Matrix( iNPoints, 1 );
        Matrix kC = new Matrix( iNPoints, 1 );
        Matrix kS = new Matrix( iNPoints, 1 );
        int position;
        for ( int i = 0; i < iNPoints; i++ )
        {
            /* kX and kY store the x and y coordinates of the snake: */ 
            if ( (m_adX != null) && (m_adY != null) )
            {
                /* Not first snake, use snake from previous frame: */
                kX.set( i, 0, m_adX[i] );
                kY.set( i, 0, m_adY[i] );
            }
            else
            {
                /* First snake (user-defined): */
                kX.set( i, 0, xPoints[i+1] );
                kY.set( i, 0, yPoints[i+1] );
                /* Set initial snake values: */
                m_kPreviousSnake.addPoint(Math.round((float)kX.get(i, 0)),
                                          Math.round((float)kY.get(i, 0)));
            }

            /* Constrained Gradient Vector Flow: Check if the new point is
             * inside the initial snake, if so set the value of the Fx and
             * Fy to be the velocity vector, otherwise use the calculated
             * GVF : */
            if ( m_bUseVelocity && m_kPreviousSnake.contains( kX.get(i,0), kY.get(i,0) ) )
            {
                kFx.set( i, 0, m_kVelocity.x );
                kFy.set( i, 0, m_kVelocity.y );
            }
            else
            {
                /* kFx and kFy store the GVF for each of the points on the snake
                 * (updated each time the snake points move) */
                position = (int) kX.get(i,0) + (xDim * (int) kY.get(i,0));
                kFx.set( i, 0,
                         getBilinear(position,
                                     (float)(kX.get(i,0) - (int) kX.get(i,0)),
                                     (float)(kY.get(i,0) - (int) kY.get(i,0)),
                                     extents, u) );
                kFy.set( i, 0,
                         getBilinear(position,
                                     (float)(kX.get(i,0) - (int) kX.get(i,0)),
                                     (float)(kY.get(i,0) - (int) kY.get(i,0)),
                                     extents, v) );
            }

            /* kC and kS are the cosine and sine functions of the snake
             * point(i), calculated once and stored: */
            kC.set( i, 0, Math.cos( (2 * Math.PI * i) / (float)iNPoints ) );
            kS.set( i, 0, Math.sin( (2 * Math.PI * i) / (float)iNPoints ) );
        }
        /* dXBar = the average x-position over all points on the snake: */
        double dXBar = kX.norm1() / (float)iNPoints;
        /* dYBar = the average y-position over all points on the snake: */
        double dYBar = kY.norm1() / (float)iNPoints;

        /* Store the center point of the current snake: */
        Point2d kCenter = new Point2d( dXBar, dYBar );

        /* kXBarM is the value of dXBar replicated in a Nx1 matrix: */ 
        Matrix kXBarM = new Matrix( iNPoints, 1 );
        /* kYBarM is the value of dYBar replicated in a Nx1 matrix: */ 
        Matrix kYBarM = new Matrix( iNPoints, 1 );

        /* Equation 11: */
        /* kR used to compute the average radius: */
        Matrix kR = new Matrix( iNPoints, 1 );
        for ( int i = 0; i < iNPoints; i++ )
        {
            /* Equation 11: */
            kR.set( i, 0,
                          Math.sqrt( (kX.get( i, 0 ) - dXBar) *
                                     (kX.get( i, 0 ) - dXBar) +
                                     (kY.get( i, 0 ) - dYBar) *
                                     (kY.get( i, 0 ) - dYBar)   ) );
            kXBarM.set( i, 0, dXBar );
            kYBarM.set( i, 0, dYBar );
        }
        /* Equation 11: */
        /* dRBar is the averate radius over all points on the snake: */
        double dRBar = kR.norm1() / (float)iNPoints;

        /* Equations 22, 23, 24: */
        /* kXr and kYr: */
        Matrix kXr = new Matrix( iNPoints, 1 );
        Matrix kYr = new Matrix( iNPoints, 1 );
        for ( int i = 0; i < iNPoints; i++ )
        {
            kXr.set( i, 0, (kX.get( i, 0 ) - dXBar)/kR.get( i, 0 ) );
            kYr.set( i, 0, (kY.get( i, 0 ) - dYBar)/kR.get( i, 0 ) );
        }

        /* Equations 15, 16: */
        /* kDx and kDy represent the distance between points on the
         * contour: */
        Matrix kDx = new Matrix( iNPoints, 1 );
        Matrix kDy = new Matrix( iNPoints, 1 );
        for ( int i = 0; i < iNPoints; i++ )
        {
            int iIndexI = ( i + 1 ) >= iNPoints ? 0: i + 1;
            kDx.set( i, 0, dRBar * (kC.get( i, 0 ) - kC.get( iIndexI, 0 )) );
            kDy.set( i, 0, dRBar * (kS.get( i, 0 ) - kS.get( iIndexI, 0 )) );
        }

        /* Equations 17, 18, 19: */
        /* Matrix kH and kG are NxN matrices: */
        Matrix kH = new Matrix( iNPoints, iNPoints, 0 );
        Matrix kG = new Matrix( iNPoints, iNPoints, 0 );
        for ( int i = 0; i < iNPoints; i++ )
        {
            for ( int j = 0; j < iNPoints; j++ )
            {
                if ( i == j )
                {
                    kH.set( i, j, 2 );
                    int iIndexI = ( i - 1 ) < 0 ? iNPoints - 1: i - 1;
                    int iIndexJ = ( j - 1 ) < 0 ? iNPoints - 1: j - 1;
                    kH.set( iIndexI, j, -1 );
                    kH.set( i, iIndexJ, -1 );

                    kG.set( i, j, 1 );
                    kG.set( i, iIndexJ, -1 );
                }
            }
        }        
        /* Equation 24: Calculate and store Q matrix: */
        double dDeltaT = 1.0/((double)boundaryIterations);
        Matrix kQ = Matrix.identity( iNPoints, iNPoints );
        kQ.timesEquals(  dDeltaT );
        Matrix kLambda1 = Matrix.identity( iNPoints, iNPoints );
        kLambda1.timesEquals( m_dLambda1 );
        kQ.plusEquals( kLambda1 );
        kH.timesEquals( m_dLambda3 );
        kQ.plusEquals( kH );
        kQ.invert();
        
        /* Iterate and minimize the constraints to find the optimal snake: */
        for (int t = 0; (t < boundaryIterations) && (!threadStopped); t++)
        {
            /* Calculate lambda1 * ( xbar + rbar * C ): */
            /* Calculate lambda1 * ( ybar + rbar * S ): */
            Matrix kRC = kC.times( dRBar );
            Matrix kRS = kS.times( dRBar );
            Matrix kLXRC = kXBarM.plus( kRC );
            Matrix kLYRC = kYBarM.plus( kRS );
            kLXRC.timesEquals( m_dLambda1 );
            kLYRC.timesEquals( m_dLambda1 );
            
            /* Calculate lambda2 * ( rbar - K ) * xr: */
            /* Calculate lambda2 * ( rbar - K ) * yr: */
            kXr.timesEquals( m_dLambda2 * ( dRBar - m_dK ) );
            kYr.timesEquals( m_dLambda2 * ( dRBar - m_dK ) );

            /* Calculate lambda3 * G * dx: */
            kDx = kG.times( kDx ); 
            kDx.timesEquals( m_dLambda3 );
            /* Calculate lambda3 * G * dy: */
            kDy = kG.times( kDy ); 
            kDy.timesEquals( m_dLambda3 );

            /* Xt+1 = Q-1 ( 1/dt * Xt + fx + lambda1 * (xbar + rbar * C) -
             * lambda2 (rbar - k) * xr + lambda3 * G * dx ) */
            Matrix kXInc = kX.times( dDeltaT );
            kXInc.plusEquals( kFx );
            kXInc.plusEquals( kLXRC );
            kXInc.minusEquals( kXr );
            kXInc.plusEquals( kDx );
            kXInc = kQ.times( kXInc );

            /* Yt+1 = Q-1 ( 1/dt * Yt + fy + lambda1 * (ybar + rbar * S) -
             * lambda2 (rbar - k) * yr + lambda3 * G * dy ) */
            Matrix kYInc = kY.times( dDeltaT );
            kYInc.plusEquals( kFy );
            kYInc.plusEquals( kLYRC );
            kYInc.minusEquals( kYr );
            kYInc.plusEquals( kDy );
            kYInc = kQ.times( kYInc );

            /* Copy into X, Y: */
            try {
                kX.finalize();
                System.gc();
            } catch ( Throwable error ) {}
            kX = kXInc;
            try {
                kY.finalize();
                System.gc();
            } catch ( Throwable error ) {}
            kY = kYInc;
            
            /* Update vars: */
            dXBar = kX.norm1() / (float)iNPoints;
            dYBar = kY.norm1() / (float)iNPoints;
            for ( int i = 0; i < iNPoints; i++ )
            {
                /* Constrained Gradient Vector Flow: Check if the new point is
                 * inside the initial snake, if so set the value of the Fx and
                 * Fy to be the velocity vector, otherwise use the calculated
                 * GVF : */
                if ( m_bUseVelocity && m_kPreviousSnake.contains( kX.get(i,0), kY.get(i,0) ) )
                {
                    kFx.set( i, 0, m_kVelocity.x );
                    kFy.set( i, 0, m_kVelocity.y );
                }
                else
                {
                    position = (int) kX.get(i,0) + (xDim * (int) kY.get(i,0));
                    kFx.set( i, 0,
                             getBilinear(position,
                                         (float)(kX.get(i,0) - (int) kX.get(i,0)),
                                         (float)(kY.get(i,0) - (int) kY.get(i,0)),
                                         extents, u) );
                    kFy.set( i, 0,
                             getBilinear(position,
                                         (float)(kX.get(i,0) - (int) kX.get(i,0)),
                                         (float)(kY.get(i,0) - (int) kY.get(i,0)),
                                         extents, v) );
                    kR.set( i, 0,
                            Math.sqrt( (kX.get( i, 0 ) - dXBar) *
                                       (kX.get( i, 0 ) - dXBar) +
                                       (kY.get( i, 0 ) - dYBar) *
                                       (kY.get( i, 0 ) - dYBar)   ) );
                    kXBarM.set( i, 0, dXBar );
                    kYBarM.set( i, 0, dYBar );
                }
            }
            dRBar = kR.norm1() / (float)iNPoints;
            
            for ( int i = 0; i < iNPoints; i++ )
            {
                kXr.set( i, 0, (kX.get( i, 0 ) - dXBar)/kR.get( i, 0 ) );
                kYr.set( i, 0, (kY.get( i, 0 ) - dYBar)/kR.get( i, 0 ) );
            }
            for ( int i = 0; i < iNPoints; i++ )
            {
                int iIndexI = ( i + 1 ) >= iNPoints ? 0: i + 1;
                kDx.set( i, 0, dRBar * (kC.get( i, 0 ) - kC.get( iIndexI, 0 )) );
                kDy.set( i, 0, dRBar * (kS.get( i, 0 ) - kS.get( iIndexI, 0 )) );
            }
        }

        /* Store the final results to initialize snake for the next frame: */
        if ( m_adX == null )
        {
            m_adX = new double[ iNPoints ];
            m_adY = new double[ iNPoints ];
        }
        /* Copy the final results into the snake: */
        resultGon.reset();
        m_kPreviousSnake.reset();
        for ( int i = 0; i < iNPoints; i++ )
        {
            resultGon.addPoint(Math.round((float)kX.get(i, 0)),
                               Math.round((float)kY.get(i, 0)));
            m_kPreviousSnake.addPoint(Math.round((float)kX.get(i, 0)),
                                      Math.round((float)kY.get(i, 0)));
            m_adX[i] = kX.get(i, 0);
            m_adY[i] = kY.get(i, 0);
        }
        /* update velocity: */
        dXBar = kX.norm1() / (float)iNPoints;
        dYBar = kY.norm1() / (float)iNPoints;
        m_kVelocity.x = dXBar - kCenter.x;
        m_kVelocity.y = dYBar - kCenter.y;
        m_kVelocity.normalize();
        return;
    }
}
