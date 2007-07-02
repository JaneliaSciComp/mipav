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

package gov.nih.mipav.view.WildMagic.LibApplications.OpenGLApplication;

import java.awt.event.*;
import java.util.Calendar;

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;

public abstract class JavaApplication3 extends JavaApplication
    implements KeyListener, MouseListener, MouseMotionListener
{
    /**
     * Creates a new JavaApplication3.
     * @param acWindowTitle, the window title
     * @param iXPosition, window screen x-position
     * @param iYPosition, window screen y-position
     * @param iWidth, window width
     * @param iHeight, window height
     * @param rkBackgroundColor, background color
     */
    public JavaApplication3 ( final String acWindowTitle, int iXPosition,
                              int iYPosition, int iWidth, int iHeight,
                              final ColorRGBA rkBackgroundColor )
    {
        super(acWindowTitle,iXPosition,iYPosition,iWidth,iHeight, rkBackgroundColor);
    }

    /** 
     * OnInitialize callback, called when the application is created.
     */
    public boolean OnInitialize ()
    {
        if (!super.OnInitialize())
        {
            return false;
        }

        m_spkCamera = new Camera();
        m_pkRenderer.SetCamera(m_spkCamera);
        m_spkMotionObject = null;
        return true;
    }

    /** 
     * OnTerminate callback, called when the application terminates.
     */
    public void OnTerminate ()
    {
        m_pkRenderer.SetCamera(null);
        m_spkCamera = null;
        m_spkMotionObject = null;
        super.OnTerminate();
    }

    /** 
     * OnDisplay callback, called when the application window is displayed.
     */
    public void OnDisplay ()
    {
        if (m_pkRenderer != null)
        {
            OnIdle();
        }
    }

    /** 
     * keyPressed callback.
     * @param kKey, the KeyEvent triggering the callback.
     */
    public void keyPressed(KeyEvent kKey)
    {
        super.keyPressed(kKey);

        char ucKey = kKey.getKeyChar();

        // standard keys
        switch (ucKey)
        {
        case 't':  // slower camera translation
            if (m_bCameraMoveable)
            {
                m_fTrnSpeed /= m_fTrnSpeedFactor;
            }
            return;
        case 'T':  // faster camera translation
            if (m_bCameraMoveable)
            {
                m_fTrnSpeed *= m_fTrnSpeedFactor;
            }
            return;
        case 'r':  // slower camera rotation
            if (m_bCameraMoveable)
            {
                m_fRotSpeed /= m_fRotSpeedFactor;
            }
            return;
        case 'R':  // faster camera rotation
            if (m_bCameraMoveable)
            {
                m_fRotSpeed *= m_fRotSpeedFactor;
            }
            return;
        case '?':  // reset the timer
            ResetTime();
            return;
        };

        if (m_bCameraMoveable)
        {
            int iKey = kKey.getKeyCode();
            if ((iKey == KeyEvent.VK_LEFT) || (iKey == KeyEvent.VK_KP_LEFT))
            {
                m_bLArrowPressed = true;
                return;
            }
            if ((iKey == KeyEvent.VK_RIGHT) || (iKey == KeyEvent.VK_KP_RIGHT))
            {
                m_bRArrowPressed = true;
                return;
            }
            if ((iKey == KeyEvent.VK_UP) || (iKey == KeyEvent.VK_KP_UP))
            {
                m_bUArrowPressed = true;
                return;
            }
            if ((iKey == KeyEvent.VK_DOWN) || (iKey == KeyEvent.VK_KP_DOWN))
            {
                m_bDArrowPressed = true;
                return;
            }
            if (iKey == KeyEvent.VK_PAGE_UP)
            {
                m_bPgUpPressed = true;
                return;
            }
            if (iKey == KeyEvent.VK_PAGE_DOWN)
            {
                m_bPgDnPressed = true;
                return;
            }
            if (iKey == KeyEvent.VK_HOME)
            {
                m_bHomePressed = true;
                return;
            }
            if (iKey == KeyEvent.VK_END)
            {
                m_bEndPressed = true;
                return;
            }
        }

        if (m_spkMotionObject != null)
        {
            int iKey = kKey.getKeyCode();
            if (iKey == KeyEvent.VK_F1)
            {
                m_iDoRoll = -1;
                return;
            }
            if (iKey == KeyEvent.VK_F2)
            {
                m_iDoRoll = 1;
                return;
            }
            if (iKey == KeyEvent.VK_F3)
            {
                m_iDoYaw = -1;
                return;
            }
            if (iKey == KeyEvent.VK_F4)
            {
                m_iDoYaw = 1;
                return;
            }
            if (iKey == KeyEvent.VK_F5)
            {
                m_iDoPitch = -1;
                return;
            }
            if (iKey == KeyEvent.VK_F6)
            {
                m_iDoPitch = 1;
                return;
            }
        }

        return;
    }

    /** 
     * keyReleased callback.
     * @param kKey, the KeyEvent triggering the callback.
     */
    public void keyReleased(KeyEvent kKey)
    {
        int iKey = kKey.getKeyCode();
        if (m_bCameraMoveable)
        {
            if ((iKey == KeyEvent.VK_LEFT) || (iKey == KeyEvent.VK_KP_LEFT))
            {
                m_bLArrowPressed = false;
                return;
            }
            if ((iKey == KeyEvent.VK_RIGHT) || (iKey == KeyEvent.VK_KP_RIGHT))
            {
                m_bRArrowPressed = false;
                return;
            }
            if ((iKey == KeyEvent.VK_UP) || (iKey == KeyEvent.VK_KP_UP))
            {
                m_bUArrowPressed = false;
                return;
            }
            if ((iKey == KeyEvent.VK_DOWN) || (iKey == KeyEvent.VK_KP_DOWN))
            {
                m_bDArrowPressed = false;
                return;
            }
            if (iKey == KeyEvent.VK_PAGE_UP)
            {
                m_bPgUpPressed = false;
                return;
            }
            if (iKey == KeyEvent.VK_PAGE_DOWN)
            {
                m_bPgDnPressed = false;
                return;
            }
            if (iKey == KeyEvent.VK_HOME)
            {
                m_bHomePressed = false;
                return;
            }
            if (iKey == KeyEvent.VK_END)
            {
                m_bEndPressed = false;
                return;
            }
        }

        if (m_spkMotionObject != null)
        {
            if (iKey == KeyEvent.VK_F1)
            {
                m_iDoRoll = 0;
                return;
            }
            if (iKey == KeyEvent.VK_F2)
            {
                m_iDoRoll = 0;
                return;
            }
            if (iKey == KeyEvent.VK_F3)
            {
                m_iDoYaw = 0;
                return;
            }
            if (iKey == KeyEvent.VK_F4)
            {
                m_iDoYaw = 0;
                return;
            }
            if (iKey == KeyEvent.VK_F5)
            {
                m_iDoPitch = 0;
                return;
            }
            if (iKey == KeyEvent.VK_F6)
            {
                m_iDoPitch = 0;
                return;
            }
        }

        return;
    }

    /** 
     * Initialize Camara Motion
     * @param fTrnSpeed, camera translation speed
     * @param fRotSpeed, camera rotation speed
     */
    protected void InitializeCameraMotion (float fTrnSpeed, float fRotSpeed) 
    {
        InitializeCameraMotion( fTrnSpeed, fRotSpeed, 2.0f, 2.0f );
    }
    
    /** 
     * Initialize Camara Motion
     * @param fTrnSpeed, camera translation speed
     * @param fRotSpeed, camera rotation speed
     * @param fTrnSpeedFactor, camera translation speed factor
     * @param fRotSpeedFactor, camera rotation speed factor
     */
    protected void InitializeCameraMotion (float fTrnSpeed, float fRotSpeed,
                                           float fTrnSpeedFactor, float fRotSpeedFactor)
    {
        m_bCameraMoveable = true;

        m_fTrnSpeed = fTrnSpeed;
        m_fRotSpeed = fRotSpeed;
        m_fTrnSpeedFactor = fTrnSpeedFactor;
        m_fRotSpeedFactor = fRotSpeedFactor;

        m_akWorldAxis[0] = m_spkCamera.GetDVector();
        m_akWorldAxis[1] = m_spkCamera.GetUVector();
        m_akWorldAxis[2] = m_spkCamera.GetRVector();
    }

    /** 
     * Camara Motion
     * @return true when the camera has moved, false if the camera has not
     * moved
     */
    protected boolean MoveCamera ()
    {
        if (!m_bCameraMoveable)
        {
            return false;
        }

        boolean bMoved = false;

        if (m_bUArrowPressed)
        {
            MoveForward();
            bMoved = true;
        }

        if (m_bDArrowPressed)
        {
            MoveBackward();
            bMoved = true;
        }

        if (m_bHomePressed)
        {
            MoveUp();
            bMoved = true;
        }

        if (m_bEndPressed)
        {
            MoveDown();
            bMoved = true;
        }

        if (m_bLArrowPressed)
        {
            TurnLeft();
            bMoved = true;
        }

        if (m_bRArrowPressed)
        {
            TurnRight();
            bMoved = true;
        }

        if (m_bPgUpPressed)
        {
            LookUp();
            bMoved = true;
        }

        if (m_bPgDnPressed)
        {
            LookDown();
            bMoved = true;
        }

        return bMoved;
    }

    /** 
     * Moves the camera forward.
     */
    protected void MoveForward ()
    {
        Vector3f kLoc = m_spkCamera.GetLocation();
        kLoc.addEquals( m_akWorldAxis[0].scale(m_fTrnSpeed) );
        m_spkCamera.SetLocation(kLoc);
        System.err.println( kLoc.X() + " " + kLoc.Y() + " " + kLoc.Z() );
    }

    /** 
     * Moves the camera backward.
     */
    protected void MoveBackward ()
    {
        Vector3f kLoc = m_spkCamera.GetLocation();
        kLoc.subEquals( m_akWorldAxis[0].scale(m_fTrnSpeed) );
        m_spkCamera.SetLocation(kLoc);
    }

    /** 
     * Moves the camera up.
     */
    protected void MoveUp ()
    {
        Vector3f kLoc = m_spkCamera.GetLocation();
        kLoc.addEquals( m_akWorldAxis[1].scale(m_fTrnSpeed));
        m_spkCamera.SetLocation(kLoc);
    }

    /** 
     * Moves the camera down.
     */
    protected void MoveDown ()
    {
        Vector3f kLoc = m_spkCamera.GetLocation();
        kLoc.subEquals( m_akWorldAxis[1].scale(m_fTrnSpeed) );
        m_spkCamera.SetLocation(kLoc);
    }

    /** 
     * Moves the camera right.
     */
    protected void MoveRight ()
    {
        Vector3f kLoc = m_spkCamera.GetLocation();
        kLoc.addEquals( m_akWorldAxis[2].scale(m_fTrnSpeed));
        m_spkCamera.SetLocation(kLoc);
    }

    /** 
     * Moves the camera left.
     */
    protected void MoveLeft ()
    {
        Vector3f kLoc = m_spkCamera.GetLocation();
        kLoc.subEquals( m_akWorldAxis[2].scale(m_fTrnSpeed) );
        m_spkCamera.SetLocation(kLoc);
    }

    /** 
     * Turns the camera left.
     */
    protected void TurnLeft ()
    {
        Matrix3f kIncr = new Matrix3f(m_akWorldAxis[1],m_fRotSpeed);
        m_akWorldAxis[0] = kIncr.mult(m_akWorldAxis[0]);
        m_akWorldAxis[2] = kIncr.mult(m_akWorldAxis[2]);

        Vector3f kDVector = kIncr.mult(m_spkCamera.GetDVector());
        Vector3f kUVector = kIncr.mult(m_spkCamera.GetUVector());
        Vector3f kRVector = kIncr.mult(m_spkCamera.GetRVector());
        m_spkCamera.SetAxes(kDVector,kUVector,kRVector);
    }

    /** 
     * Turns the camera right.
     */
    protected void TurnRight ()
    {
        Matrix3f kIncr = new Matrix3f(m_akWorldAxis[1],-m_fRotSpeed);
        m_akWorldAxis[0] = kIncr.mult(m_akWorldAxis[0]);
        m_akWorldAxis[2] = kIncr.mult(m_akWorldAxis[2]);

        Vector3f kDVector = kIncr.mult(m_spkCamera.GetDVector());
        Vector3f kUVector = kIncr.mult(m_spkCamera.GetUVector());
        Vector3f kRVector = kIncr.mult(m_spkCamera.GetRVector());
        m_spkCamera.SetAxes(kDVector,kUVector,kRVector);
    }

    /** 
     * Causes the camera to look up.
     */
    protected void LookUp ()
    {
        Matrix3f kIncr = new Matrix3f(m_akWorldAxis[2],m_fRotSpeed);

        Vector3f kDVector = kIncr.mult(m_spkCamera.GetDVector());
        Vector3f kUVector = kIncr.mult(m_spkCamera.GetUVector());
        Vector3f kRVector = kIncr.mult(m_spkCamera.GetRVector());
        m_spkCamera.SetAxes(kDVector,kUVector,kRVector);
    }

    /** 
     * Causes the camera to look down.
     */
    protected void LookDown ()
    {
        Matrix3f kIncr = new Matrix3f(m_akWorldAxis[2],-m_fRotSpeed);

        Vector3f kDVector = kIncr.mult(m_spkCamera.GetDVector());
        Vector3f kUVector = kIncr.mult(m_spkCamera.GetUVector());
        Vector3f kRVector = kIncr.mult(m_spkCamera.GetRVector());
        m_spkCamera.SetAxes(kDVector,kUVector,kRVector);
    }

    /** Camera object. */
    protected Camera m_spkCamera;

    /** camera motion parameters */
    protected Vector3f[] m_akWorldAxis =
        new Vector3f[]{ new Vector3f( Vector3f.ZERO ),
                        new Vector3f( Vector3f.ZERO ),
                        new Vector3f( Vector3f.ZERO ) };

    protected float m_fTrnSpeed = 0.0f;
    protected float m_fRotSpeed = 0.0f;
    protected float m_fTrnSpeedFactor = 2.0f;
    protected float m_fRotSpeedFactor = 2.0f;
    protected boolean m_bUArrowPressed = false;
    protected boolean m_bDArrowPressed = false;
    protected boolean m_bLArrowPressed = false;
    protected boolean m_bRArrowPressed = false;
    protected boolean m_bPgUpPressed = false;
    protected boolean m_bPgDnPressed = false;
    protected boolean m_bHomePressed = false;
    protected boolean m_bEndPressed = false;
    protected boolean m_bCameraMoveable = false;
    protected int m_iMouseButton = 0;

    /** Initializes Object Motion */
    protected void InitializeObjectMotion (Spatial pkMotionObject)
    {
        m_spkMotionObject = pkMotionObject;
    }

    /** Moves object.
     * @return true when the object has moved, false otherwise.
     */
    protected boolean MoveObject ()
    {
        // The coordinate system in which the rotations are applied is that of
        // the object's parent, if it has one.  The parent's world rotation
        // matrix is R, of which the columns are the coordinate axis directions.
        // Column 0 is "direction", column 1 is "up", and column 2 is "right".
        // If the object does not have a parent, the world coordinate axes are
        // used, in which case the rotation matrix is I, the identity.  Column 0
        // is (1,0,0) and is "direction", column 1 is (0,1,0) and is "up", and
        // column 2 is (0,0,1) and is "right".  This choice is consistent with
        // the use of rotations in the Camera and Light classes to store
        // coordinate axes.
        //
        // Roll is about the "direction" axis, yaw is about the "up" axis, and
        // pitch is about the "right" axis.

        if (!m_bCameraMoveable || (m_spkMotionObject == null))
        {
            return false;
        }

        // Check if the object has been moved by the virtual trackball.
        if (m_bTrackBallDown)
        {
            return true;
        }

        // Check if the object has been moved by the function keys.
        Spatial pkParent = m_spkMotionObject.GetParent();
        Vector3f kAxis;
        float fAngle;
        Matrix3f kRot, kIncr = new Matrix3f();

        if (m_iDoRoll != 0)
        {
            kRot = m_spkMotionObject.Local.GetRotate();

            fAngle = m_iDoRoll*m_fRotSpeed;
            if (pkParent != null)
            {
                kAxis = pkParent.World.GetRotate().GetColumn(0);
            }
            else
            {
                kAxis = Vector3f.UNIT_X;
            }

            kIncr.FromAxisAngle(kAxis,fAngle);
            kRot = kIncr.mult(kRot);
            kRot.Orthonormalize();
            m_spkMotionObject.Local.SetRotate(kRot);
            return true;
        }

        if (m_iDoYaw != 0)
        {
            kRot = m_spkMotionObject.Local.GetRotate();

            fAngle = m_iDoYaw*m_fRotSpeed;
            if (pkParent != null)
            {
                kAxis = pkParent.World.GetRotate().GetColumn(1);
            }
            else
            {
                kAxis = Vector3f.UNIT_Y;
            }

            kIncr.FromAxisAngle(kAxis,fAngle);
            kRot = kIncr.mult(kRot);
            kRot.Orthonormalize();
            m_spkMotionObject.Local.SetRotate(kRot);
            return true;
        }

        if (m_iDoPitch != 0)
        {
            kRot = m_spkMotionObject.Local.GetRotate();

            fAngle = m_iDoPitch*m_fRotSpeed;
            if (pkParent != null)
            {
                kAxis = pkParent.World.GetRotate().GetColumn(2);
            }
            else
            {
                kAxis = Vector3f.UNIT_Z;
            }

            kIncr.FromAxisAngle(kAxis,fAngle);
            kRot = kIncr.mult(kRot);
            kRot.Orthonormalize();
            m_spkMotionObject.Local.SetRotate(kRot);
            return true;
        }

        return false;
    }

    /**
     * Rotate the the object with a virtual trackball
     * @param fX0, start mouse x-position
     * @param fY0, start mouse y-position
     * @param fX1, end mouse x-position
     * @param fY1, end mouse y-position
     */
    protected void RotateTrackBall (float fX0, float fY0, float fX1, float fY1)
    {
        if ((fX0 == fX1 && fY0 == fY1) || (m_spkCamera==null))  // nothing to rotate
            return;
        
        // get first vector on sphere
        float fLength = (float)Math.sqrt(fX0*fX0+fY0*fY0), fInvLength, fZ0, fZ1;
        if (fLength > 1.0f)
        {
            // outside unit disk, project onto it
            fInvLength = 1.0f/fLength;
            fX0 *= fInvLength;
            fY0 *= fInvLength;
            fZ0 = 0.0f;
        }
        else
        {
            // compute point (x0,y0,z0) on negative unit hemisphere
            fZ0 = 1.0f - fX0*fX0 - fY0*fY0;
            fZ0 = (fZ0 <= 0.0f ? 0.0f : (float)Math.sqrt(fZ0));
        }
        fZ0 *= -1.0f;
        
        // use camera world coordinates, order is (D,U,R), so point is (z,y,x)
        Vector3f kVec0 = new Vector3f(fZ0,fY0,fX0);

        // get second vector on sphere
        fLength = (float)Math.sqrt(fX1*fX1+fY1*fY1);
        if (fLength > 1.0f)
        {
            // outside unit disk, project onto it
            fInvLength = 1.0f/fLength;
            fX1 *= fInvLength;
            fY1 *= fInvLength;
            fZ1 = 0.0f;
        }
        else
        {
            // compute point (x1,y1,z1) on negative unit hemisphere
            fZ1 = 1.0f - fX1*fX1 - fY1*fY1;
            fZ1 = (fZ1 <= 0.0f ? 0.0f : (float)Math.sqrt(fZ1));
        }
        fZ1 *= -1.0f;

        // use camera world coordinates, order is (D,U,R), so point is (z,y,x)
        Vector3f kVec1 = new Vector3f(fZ1,fY1,fX1);

        // create axis and angle for the rotation
        Vector3f kAxis = kVec0.Cross(kVec1);
        float fDot = kVec0.Dot(kVec1);
        float fAngle;
        if (kAxis.Normalize() > Mathf.ZERO_TOLERANCE)
        {
            fAngle = (float)Math.acos(kVec0.Dot(kVec1)); 
        }
        else  // vectors are parallel
        {
            if (fDot < 0.0f)
            {
                // rotated pi radians
                fInvLength = Mathf.InvSqrt(fX0*fX0+fY0*fY0);
                kAxis.X( fY0*fInvLength );
                kAxis.Y( -fX0*fInvLength );
                kAxis.Z( 0.0f );
                fAngle = (float)Math.PI;
            }
            else
            {
                // rotation by zero radians
                kAxis = Vector3f.UNIT_X;
                fAngle = 0.0f;
            }
        }

        // Compute the world rotation matrix implied by trackball motion.  The
        // axis vector was computed in camera coordinates.  It must be converted
        // to world coordinates.  Once again, I use the camera ordering (D,U,R).
        Vector3f kWorldAxis =
            m_spkCamera.GetDVector().scale(kAxis.X()).
            add(m_spkCamera.GetUVector().scale(kAxis.Y())).
            add(m_spkCamera.GetRVector().scale(kAxis.Z()));

        Matrix3f kTrackRotate = new Matrix3f(kWorldAxis,fAngle);

        // Compute the new local rotation.  If the object is the root of the
        // scene, the new rotation is simply the *incremental rotation* of the
        // trackball applied *after* the object has been rotated by its old
        // local rotation.  If the object is not the root of the scene, you have
        // to convert the incremental rotation by a change of basis in the
        // parent's coordinate space. 
        final Spatial pkParent = m_spkMotionObject.GetParent();
        Matrix3f kLocalRot;
        if (pkParent != null)
        {
            final Matrix3f rkPRotate = pkParent.World.GetRotate();
            kLocalRot = rkPRotate.TransposeTimes(kTrackRotate).mult( rkPRotate ).
                mult( m_kSaveRotate );
        }
        else
        {
            kLocalRot = kTrackRotate.mult(m_kSaveRotate);
        }
        kLocalRot.Orthonormalize();
        m_spkMotionObject.Local.SetRotate(kLocalRot);
    }

    /** Object rotation parameters: */
    protected Spatial m_spkMotionObject = null;
    protected int m_iDoRoll = 0, m_iDoYaw = 0, m_iDoPitch = 0;
    protected float m_fXTrack0 = 0.0f, m_fYTrack0 = 0.0f, m_fXTrack1 = 0.0f, m_fYTrack1 = 0.0f;
    protected float m_fXDrag0 = 0.0f;
    protected float m_fYDrag0 = 0.0f;
    protected Matrix3f m_kSaveRotate;
    protected boolean m_bUseTrackBall = true, m_bTrackBallDown = false;

    /** performance measurements */
    /** Resets time */
    protected void ResetTime ()
    {
        m_dLastTime = -1.0f;
    }

    /** Measure time */
    protected void MeasureTime ()
    {
        Calendar kCalendar = Calendar.getInstance();
        // start performance measurements
        if (m_dLastTime == -1.0)
        {
            m_dLastTime = kCalendar.get(Calendar.SECOND);
            m_dAccumulatedTime = 0.0;
            m_dFrameRate = 0.0;
            m_iFrameCount = 0;
            m_iAccumulatedFrameCount = 0;
            m_iTimer = m_iMaxTimer;
        }

        // accumulate the time only when the miniature time allows it
        if (--m_iTimer == 0)
        {
            double dCurrentTime = kCalendar.get(Calendar.SECOND);
            double dDelta = dCurrentTime - m_dLastTime;
            m_dLastTime = dCurrentTime;
            m_dAccumulatedTime += dDelta;
            m_iAccumulatedFrameCount += m_iFrameCount;
            m_iFrameCount = 0;
            m_iTimer = m_iMaxTimer;
        }
    }

    /** Update frame count */
    protected void UpdateFrameCount ()
    {
        m_iFrameCount++;
    }

    /** Draws the frame rate on screen
     * @param iX, the x-position for drawing the frame rate
     * @param iY, the y-position for drawing the frame rate
     * @param rkColor, the text color
     */
    protected void DrawFrameRate (int iX, int iY, final ColorRGBA rkColor)
    {
        if (m_dAccumulatedTime > 0.0)
        {
            m_dFrameRate = m_iAccumulatedFrameCount/m_dAccumulatedTime;
        }
        else
        {
            m_dFrameRate = 0.0;
        }

        String kMessage = new String( "fps: " + m_dFrameRate);
        m_pkRenderer.Draw(iX,iY,rkColor,kMessage.toCharArray());
    }

    /** Performance paramters: */
    protected double m_dLastTime = -1.0f, m_dAccumulatedTime = 0.0f, m_dFrameRate = 0.0f;
    protected int m_iFrameCount = 0, m_iAccumulatedFrameCount = 0, m_iTimer = 30, m_iMaxTimer = 30;

    /** User input: */
    public void keyTyped(KeyEvent arg0) {}

    public void mouseClicked(MouseEvent arg0) {}

    public void mouseEntered(MouseEvent arg0) {}

    public void mouseExited(MouseEvent arg0) {}

    /** Ends trackball rotation:
     * @param e, the MouseEvent
     */
    public void mouseReleased(MouseEvent e)
    {
        m_iMouseButton = 0;
        if (!m_bUseTrackBall
            ||  e.getButton() != MouseEvent.BUTTON1
            ||  (m_spkMotionObject == null))
        {
            return;
        }
        m_bTrackBallDown = false;
    }
    
    public void mouseMoved(MouseEvent arg0) {}
    
    /** Initializes trackball rotation:
     * @param e, the MouseEvent
     */
    public void mousePressed(MouseEvent e)
    {
        int iX = e.getX();
        int iY = e.getY();
        float fMult = 1.0f/(m_iWidth >= m_iHeight ? m_iHeight : m_iWidth);
        // get the starting point
        m_fXDrag0 = (2*iX-m_iWidth)*fMult;
        m_fYDrag0 = (2*(m_iHeight-1-iY)-m_iHeight)*fMult;


        m_iMouseButton = e.getButton();
        if (!m_bUseTrackBall
            ||  e.getButton() != MouseEvent.BUTTON1
            ||  (m_spkMotionObject == null))
        {
            return;
        }

        // get the starting point
        m_bTrackBallDown = true;
        m_kSaveRotate = m_spkMotionObject.Local.GetRotate();
        m_fXTrack0 = (2*iX-m_iWidth)*fMult;
        m_fYTrack0 = (2*(m_iHeight-1-iY)-m_iHeight)*fMult;
    }

    /** Rotates the object with a virtual trackball:
     * @param e, the MouseEvent
     */
    public void mouseDragged(MouseEvent e)
    {
        if (m_iMouseButton == MouseEvent.BUTTON2)
        {
            int iX = e.getX();
            int iY = e.getY();
            // get the ending point
            float fMult = 1.0f/(m_iWidth >= m_iHeight ? m_iHeight : m_iWidth);
            float fYDrag1 = (2*(m_iHeight-1-iY)-m_iHeight)*fMult;
            float fSpeed = Math.abs( fYDrag1 ); 
            float fTrnSpeedTmp = m_fTrnSpeed;
            m_fTrnSpeed = .01f + fSpeed;
            if ( fYDrag1 > m_fYDrag0 )
            {
                MoveBackward();
            }
            else if ( fYDrag1 < m_fYDrag0 )
            {
                MoveForward();
            }
            m_fTrnSpeed = fTrnSpeedTmp;
            m_fYDrag0 = fYDrag1;
        }
        if (m_iMouseButton == MouseEvent.BUTTON3)
        {
            int iX = e.getX();
            int iY = e.getY();
            // get the ending point
            float fMult = 1.0f/(m_iWidth >= m_iHeight ? m_iHeight : m_iWidth);
            float fXDrag1 = (2*iX-m_iWidth)*fMult;
            float fYDrag1 = (2*(m_iHeight-1-iY)-m_iHeight)*fMult;
          
            float fTrnSpeedTmp = m_fTrnSpeed;
            if ( fXDrag1 > m_fXDrag0 )
            {
                m_fTrnSpeed = .01f;// + Math.abs( fXDrag1 )/50f;
                MoveLeft();
            }
            else if ( fXDrag1 < m_fXDrag0 )
            {
                m_fTrnSpeed = .01f;//  + Math.abs( fXDrag1 )/50f;
                MoveRight();
            }
            if ( fYDrag1 > m_fYDrag0 )
            {
                m_fTrnSpeed = .01f;//  + Math.abs( fYDrag1 )/50f;
                MoveDown();
            }
            else if ( fYDrag1 < m_fYDrag0 )
            {
                m_fTrnSpeed = .01f;//  + Math.abs( fYDrag1 )/50f;
                MoveUp();
            }
            System.err.println(m_fTrnSpeed);
            m_fTrnSpeed = fTrnSpeedTmp;
            m_fXDrag0 = fXDrag1;
            m_fYDrag0 = fYDrag1;
        }

        if (!m_bUseTrackBall
            ||  m_iMouseButton != MouseEvent.BUTTON1
            ||  !m_bTrackBallDown
            ||  (m_spkMotionObject ==  null))
        {
            return;
        }
        int iX = e.getX();
        int iY = e.getY();
        // get the ending point
        float fMult = 1.0f/(m_iWidth >= m_iHeight ? m_iHeight : m_iWidth);
        m_fXTrack1 = (2*iX-m_iWidth)*fMult;
        m_fYTrack1 = (2*(m_iHeight-1-iY)-m_iHeight)*fMult;

        // update the object's local rotation
        RotateTrackBall(m_fXTrack0,m_fYTrack0,m_fXTrack1,m_fYTrack1);
    }

}
