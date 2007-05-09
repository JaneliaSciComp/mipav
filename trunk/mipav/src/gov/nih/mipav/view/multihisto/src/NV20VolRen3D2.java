package gov.nih.mipav.view.multihisto.src;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import java.nio.*;

public class NV20VolRen3D2 extends gluvvPrimitive {

  private int go; //if everything is ok, you can render
  int sx, sy, sz; //not used anymore, get info from meta volume
  private int[] texNames; //bricked data names
  private int[] shadeNames; //bricked shade names
  private int[] shadowName = new int[1]; //shadow texture
  private int[] qnNames; //not used qn sucks
  private int[] deptexName = new int[1]; //VG dep tex [gb]
  private int[] deptex2Name = new int[1]; //H dep tex  [ar]
  private int qnDeptexName; //not used qn sucks

  private byte[] deptex; //dep tex data  [gb]
  private byte[] deptex2; //dep tex2 data [ar]
  private byte[] gDeptex; //dep tex scaled to good sample rate [gb]
  private byte[] iDeptex; //dep tex scaled to interactive sample rate [gb]

  private byte[] qnDeptex; //not used qn sucks
  private float[] qnRef; //again not used...

  private float lastSamp; //last sample rate rendered
  private float lastGoodSamp; //last good sample rate
  private float lastInteSamp; //last interactive sample rate

  // private PBuffer * pbuff; //pbuffer for rendering light's point of view
  // private void MakeCurrent();

  //----- some shadow data ------
  private float xscale;
  private float yscale;
  private float xbias;
  private float ybias;

  private VectorMath math = new VectorMath();
  private GLU glu = new GLU();
  private gluvvGlobal gluvv;

  //////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

  public NV20VolRen3D2(gluvvGlobal _gluvv) {
    go = 0;
    gluvv = _gluvv;
  }

  public void init(GLAutoDrawable drawable)
  {
     GL gl = drawable.getGL();
          if(gluvv.mv != null ){
                  go = 1;
                  createVolumes(drawable);
                  create2DDepTex(drawable);
                  createShadowTex(drawable);
                  // pbuff ?????????????????????????????????????????????????????????????????
                  /*
                  if(!(pbuff = new PBuffer(gluvv.light.buffsz[0], gluvv.light.buffsz[1],
                                                 GLUT_SINGLE | GLUT_RGBA))){
                          System.err.println("ERROR: PBuffer intialization failed!");
                  } else {
                          pbuff.Initialize(true);
                          pbuff.MakeCurrent();
                          gl.glClearColor( 0.0f, 0.0f, 0.0f, 0.0f );
                          gl.glMatrixMode( GL.GL_PROJECTION );
                          gl.glOrtho(0,gluvv.light.buffsz[0],0,gluvv.light.buffsz[1],1,3);
                          gl.glMatrixMode(GL.GL_MODELVIEW);
                          glu.gluLookAt(0,0,2, 0,0,0, 0,1,0);
                          MakeCurrent();
                          System.err.println("PBuffer intialization succeded");
                  }
                  */
          }

          System.err.println("Using the Shadow Renderer!!!!!!!");

  }

  //================================================================= DRAW
//======================================================================

  public void draw(GLAutoDrawable drawable) {
    if (go == 0) {
      return;
    }

    renderVolume(drawable);
    // renderSlice(drawable);
  }


  //======================================================== Render Volume
//======================================================================
//========== NOTES ===========================================
// Standard brick vertex ordering:
//
//     (011)        (111)
//       6 +---------+ 7   Where 1's are the size of the brick
//        /|        /|      allong that axis
//       / |       / |
// (001)/  |(101) /  |
//   4 +---------+ 5 |
//     |   |     |   |(110) z axis
//     | 2 +-----+---+ 3    ^
//     |  /(010) |  /       |   y axis
//     | /       | /        | /
//     |/        |/         |/
//   0 +---------+ 1        +-------> x axis
//  (000)      (100)
//

  public void renderVolume(GLAutoDrawable drawable) {
    int i;
    GL gl = drawable.getGL();

    if(gluvv.picking == 1){
            return;
    }


    //-------------- Re-Scale Alpha values ---------------------------------
    if (gluvv.volren.scaleAlphas == 1) {
      if ( (lastSamp != gluvv.volren.sampleRate) || (gluvv.volren.loadTLUT == 1)) { //see if the sample rate changed
        if ( (lastGoodSamp != gluvv.volren.goodSamp) || gluvv.volren.loadTLUT == 1) { //good sample rate changed
          copyScale(gluvv.volren.goodSamp * 1 / gluvv.volren.gamma, gDeptex);
          lastGoodSamp = gluvv.volren.goodSamp;
        }
        if ( (lastInteSamp != gluvv.volren.interactSamp) || gluvv.volren.loadTLUT == 1) { //interact samp rate changed
          copyScale(gluvv.volren.interactSamp * 1 / gluvv.volren.gamma, iDeptex);
          lastInteSamp = gluvv.volren.interactSamp;
        }

        if (gluvv.volren.sampleRate == gluvv.volren.goodSamp) { //which one do we load (good)
          loadDepTex(drawable, gDeptex);
          lastSamp = gluvv.volren.goodSamp;
        }
        else if (gluvv.volren.sampleRate == gluvv.volren.interactSamp) { //(interactive)
          loadDepTex(drawable, iDeptex);
          lastSamp = gluvv.volren.interactSamp;
        }
        if (gluvv.volren.loadTLUT == 1) { //now load the transfer function
          loadDepTex(drawable, gluvv.volren.deptex2, deptex2Name[0]);
        }
        gluvv.volren.loadTLUT = 0;
      }
    }
    else { //just do gamma scale, don't update for the sample rate (for testing purposes)
      if (gluvv.volren.loadTLUT == 1) {
        copyScale(1 / gluvv.volren.gamma, gDeptex);
        loadDepTex(drawable, gDeptex);
        loadDepTex(drawable, gluvv.volren.deptex2, deptex2Name[0]);
        gluvv.volren.loadTLUT = 0;
      }
    }
    //-------------- end Re-Scale Alpha values ------------------------------

    //-------------- do dot product with clip and view dir
    float[] vdir = new float[3];
    math.subV3(vdir, gluvv.env.eye, gluvv.clip.pos);
    math.normalizeV3(vdir);
    math.normalizeV3(gluvv.clip.dir);
    float dv = math.dotV3(vdir, gluvv.clip.dir);
    float[] globalModV = new float[16]; //save original tranform
    gl.glGetFloatv(GL.GL_MODELVIEW_MATRIX, globalModV, 0); //save the world model view
    //-------------- end do dot product with clip and view dir

    gl.glPushMatrix();
    //move to the volume location
    gl.glTranslatef(gluvv.rinfo.trans[0], //translate
                 gluvv.rinfo.trans[1],
                 gluvv.rinfo.trans[2]);
    gl.glMultMatrixf(gluvv.rinfo.xform, 0); //rotate
    gl.glTranslatef( -gluvv.mv.xfSize / 2f, //center
                 -gluvv.mv.yfSize / 2f,
                 -gluvv.mv.zfSize / 2f);

    //-------------- draw clip slices --------------------------------------
    for (i = 0; i < gluvv.mv.numSubVols; ++i) {
      drawClip(drawable, i, dv, globalModV);
    }
    //-------------- end draw clip slices ----------------------------------

    //----------------------------------------------------------------------
    //-------------- render the volume -------------------------------------
    //-- first compute the axis for rendering
    float[] axis  = {0f,0f,1f,0f}; //axis[4], 3 for view direction and one for the dot

    // modify slice axis for shadowing ------
    float[] ldir = new float[3]; //figure out the light direction
    math.copyV3(ldir,gluvv.light.pos); //this is the position
    math.negateV3(ldir);               //this is the direction
    math.normalizeV3(ldir);            //normalize both vectors
    math.normalizeV3(axis);            //    light & axis
    float vdl = math.dotV3(ldir,axis);  //the dot product tells us to flip the axis

    if(vdl<=0){  //need to negate the axis
            //cerr << "back to front" << endl;
            math.negateV3(axis);
    } else {
            //cerr << "front to back" << endl;
    }
    //now compute  (v - l)/2 + l = halfway view and light
    float[] spn = new float[3];            //slice plane normal
    math.subV3(spn, axis, ldir);
    math.scaleV3(.5f,spn);
    float[] halfv = new float[3];
    math.addV3(halfv, spn, ldir);
    math.copyV3(axis, halfv);
    axis[3] = vdl; //this stores the value of the dot product
    axis[1] = -axis[1];//why? I have no idea, there must be something wrong with my math

    math.negateV3(axis);  //the actual direction for slicing

    // end modify slice axis for shadowing ------

    setupShaders(drawable);
    setupRegComb(drawable, vdl);
    setupPBuff(drawable);                  // pbuff ???????????????????????????????????????????????????????

    double[] mv = new double[16];
    gl.glGetDoublev(GL.GL_MODELVIEW_MATRIX, mv, 0); //save modelview matrix

    renderBricks(drawable, mv, axis); //RENDER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    resetShaders(drawable);
    resetRegComb(drawable);
    //-------------- end render the volume ---------------------------------
    //----------------------------------------------------------------------


    gluvv.reblend = gluvvBlend.GB_UNDER; //force gluvv.display to composite the background

    //-------------- draw clip slices --------------------------------------
    for (i = 0; i < gluvv.mv.numSubVols; ++i) {
      drawClip(drawable, i, -dv, globalModV);
    }
    //-------------- end draw clip slices ----------------------------------


    gl.glPopMatrix();

    resetShaders(drawable);
    resetRegComb(drawable);
    resetClips(drawable);

    if (gluvv.light.showView == 1) {
      gl.glEnable(GL.GL_TEXTURE_2D);
      gl.glBindTexture(GL.GL_TEXTURE_2D, shadowName[0]);
      gl.glBegin(GL.GL_QUADS);
      {
        gl.glTexCoord2f(0, 0);
        gl.glVertex3f(0, 0, 0);
        gl.glTexCoord2f(1, 0);
        gl.glVertex3f(1, 0, 0);
        gl.glTexCoord2f(1, 1);
        gl.glVertex3f(1, 1, 0);
        gl.glTexCoord2f(0, 1);
        gl.glVertex3f(0, 1, 0);
      }
      gl.glEnd();
      gl.glDisable(GL.GL_TEXTURE_2D);
    }


  }

  //======================================================== Render Bricks
//======================================================================

  public void renderBricks(GLAutoDrawable drawable, double[] mv, float[] axis)   // double mv[16], GLfloat axis[4]
  {
          int i;
          GL gl = drawable.getGL();
          if(gluvv.mv.numSubVols > 1){ //render multiple bricked volumes
                  int[] order = new int[gluvv.mv.numSubVols];   //the correct draw order
                  float[] zval = new float[gluvv.mv.numSubVols];//zvalue of each subvolume
                  float[] ref = new float[3];
                  math.copyV3(ref, axis);
                  math.negateV3(ref);
                  math.scaleV3(100, ref);

                  for(i=0; i< gluvv.mv.numSubVols; ++i){ //compute zvalues
                          Volume v = gluvv.mv.volumes[i];
                          order[i] = i;
                          float[] c = {v.xfPos + v.xfSize/2.0f, v.yfPos + v.yfSize/2.0f, v.zfPos + v.zfSize/2.0f};  // float c[3]
                          float[] center = new float[3];
                          math.translateV3W(center, mv, c);
                          float[] sub = new float[3];
                          math.subV3(sub, ref, center);
                          //zval[i] = normV3(sub);//(float)(center[0]*center[0] + center[1]*center[1] + center[2]*center[2]);
                          zval[i] = (float)(center[0]*center[0] + center[1]*center[1] + center[2]*center[2]);
                  }

                  for(i=0; i< gluvv.mv.numSubVols-1; ++i){ //now sort
                          for(int j=i+1; j<gluvv.mv.numSubVols; ++j){
                            if (axis[3] <= 0) { //back to front
                              if (zval[order[i]] < zval[order[j]]) { //test for swap
                                int tmp = order[i];
                                order[i] = order[j];
                                order[j] = tmp;
                              }
                            }
                            else { //front to back
                              if (zval[order[i]] > zval[order[j]]) { //test for swap
                                int tmp = order[i];
                                order[i] = order[j];
                                order[j] = tmp;
                              }
                            }

                          }
                  }

                  for(i=0 ; i< gluvv.mv.numSubVols; ++i){ //finaly render
                          Volume v = gluvv.mv.volumes[order[i]];

                          float sxf = v.xfSize;
                          float syf = v.yfSize;
                          float szf = v.zfSize;
                          float hpx = 1.0f/(gluvv.mv.xiSize*2.0f);
                          float hpy = 1.0f/(gluvv.mv.yiSize*2.0f);
                          float hpz = 1.0f/(gluvv.mv.ziSize*2.0f);

                          float[][] vo = {{0,0,0},{sxf,0,0},{0,syf,0},{sxf,syf,0},{0,0,szf},{sxf,0,szf},{0,syf,szf},{sxf,syf,szf}};  // vo[8][3]
                          float[][] tx = {{hpx,hpy,hpz},{1-hpx,hpy,hpz},{hpx,1-hpy,hpz},{1-hpx,1-hpy,hpz},
                                                                          {hpx,hpy,1-hpz},{1-hpx,hpy,1-hpz},{hpx,1-hpy,1-hpz},{1-hpx,1-hpy,1-hpz}};
                                               // tx[8][3]

                          gl.glPushMatrix();
                          setupClips(order[i],vo,tx);

                          gl.glTranslatef(v.xfPos, v.yfPos, v.zfPos);
                          render3DVA(drawable, gluvv.volren.sampleRate,mv,order[i],vo,tx,axis);

                          gl.glPopMatrix();
                  }

          } else { //just render one volume!
                  float sxf = gluvv.mv.xfSize;
                  float syf = gluvv.mv.yfSize;
                  float szf = gluvv.mv.zfSize;

                  float[][] vo = {{0,0,0},{sxf,0,0},{0,syf,0},{sxf,syf,0},{0,0,szf},{sxf,0,szf},{0,syf,szf},{sxf,syf,szf}}; // vo[8][3]
                  float[][] tx = {{0,0,0},{1  ,0,0},{0,1  ,0},{1  ,1  ,0},{0,0,1  },{1  ,0,1  },{0,1  ,1  },{1  ,1  ,1  }};  // tx[8][3]

                  setupClips(0,vo,tx);

                  render3DVA(drawable, gluvv.volren.sampleRate,mv,0,vo,tx,axis);

          }
  }

  //========================================================== Setup Clips
//======================================================================
  private void setupClips(int vol, float[][] vo, float[][] tx) { // int vol, float vo[8][3], float tx[8][3]
    Volume v = gluvv.mv.volumes[vol];

    if (gluvv.clip.on == 1 && gluvv.clip.ortho == 1) {
      float[] cp = new float[3];
      cp[0] = gluvv.clip.vpos[0] > v.xfPos ?
          (gluvv.clip.vpos[0] < v.xfSize + v.xfPos ?
           gluvv.clip.vpos[0] - v.xfPos : v.xfSize) : 0;
      cp[1] = gluvv.clip.vpos[1] > v.yfPos ?
          (gluvv.clip.vpos[1] < v.yfSize + v.yfPos ?
           gluvv.clip.vpos[1] - v.yfPos : v.yfSize) : 0;
      cp[2] = gluvv.clip.vpos[2] > v.zfPos ?
          (gluvv.clip.vpos[2] < v.zfSize + v.zfPos ?
           gluvv.clip.vpos[2] - v.zfPos : v.zfSize) : 0;
      if ( gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisXPos ) {
          tx[1][0] = cp[0] / v.xfSize;
          tx[3][0] = cp[0] / v.xfSize;
          tx[5][0] = cp[0] / v.xfSize;
          tx[7][0] = cp[0] / v.xfSize;
          vo[1][0] = cp[0];
          vo[3][0] = cp[0];
          vo[5][0] = cp[0];
          vo[7][0] = cp[0];
      } else if ( gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisXNeg ) {
          tx[0][0] = cp[0] / v.xfSize;
          tx[2][0] = cp[0] / v.xfSize;
          tx[4][0] = cp[0] / v.xfSize;
          tx[6][0] = cp[0] / v.xfSize;
          vo[0][0] = cp[0];
          vo[2][0] = cp[0];
          vo[4][0] = cp[0];
          vo[6][0] = cp[0];
      } else if ( gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisYPos ) {
          tx[2][1] = cp[1] / v.yfSize;
          tx[3][1] = cp[1] / v.yfSize;
          tx[6][1] = cp[1] / v.yfSize;
          tx[7][1] = cp[1] / v.yfSize;
          vo[2][1] = cp[1];
          vo[3][1] = cp[1];
          vo[6][1] = cp[1];
          vo[7][1] = cp[1];
      } else if ( gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisYNeg ) {
          tx[0][1] = cp[1] / v.yfSize;
          tx[1][1] = cp[1] / v.yfSize;
          tx[4][1] = cp[1] / v.yfSize;
          tx[5][1] = cp[1] / v.yfSize;
          vo[0][1] = cp[1];
          vo[1][1] = cp[1];
          vo[4][1] = cp[1];
          vo[5][1] = cp[1];
      } else if ( gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisZPos ) {
          tx[4][2] = cp[2] / v.zfSize;
          tx[5][2] = cp[2] / v.zfSize;
          tx[6][2] = cp[2] / v.zfSize;
          tx[7][2] = cp[2] / v.zfSize;
          vo[4][2] = cp[2];
          vo[5][2] = cp[2];
          vo[6][2] = cp[2];
          vo[7][2] = cp[2];
      } else if ( gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisZNeg ) {
          tx[0][2] = cp[2] / v.zfSize;
          tx[1][2] = cp[2] / v.zfSize;
          tx[2][2] = cp[2] / v.zfSize;
          tx[3][2] = cp[2] / v.zfSize;
          vo[0][2] = cp[2];
          vo[1][2] = cp[2];
          vo[2][2] = cp[2];
          vo[3][2] = cp[2];
      }

    }

  }


  //============================================================ drawClips
//======================================================================
  private void drawClip(GLAutoDrawable drawable, int vol, float dv, float[] wmv) {   // float wmv[16]
    GL gl = drawable.getGL();
    if ( (gluvv.clip.ortho == 0)) {
      if ( gluvv.dmode == gluvvDataMode.GDM_V1 ||
           gluvv.dmode == gluvvDataMode.GDM_V1G ||
           gluvv.dmode == gluvvDataMode.GDM_V2 ||
           gluvv.dmode == gluvvDataMode.GDM_VGH_VG ||
           gluvv.dmode == gluvvDataMode.GDM_VGH_V ) {
          System.err.println("clipping");
          gl.glActiveTexture(GL.GL_TEXTURE2);
          { //3rd & 4th axis, or clipping
            gl.glEnable(GL.GL_TEXTURE_2D);
            gl.glDisable(GL.GL_TEXTURE_3D);
            gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_NONE);
            gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_CULL_FRAGMENT_NV);
          }
          gl.glPushMatrix();
          { //this sets the clipping plane
            gl.glLoadIdentity(); //clips are set in world space
            gl.glMultMatrixf(wmv, 0); //origional world space coords
            gl.glTranslatef(gluvv.clip.pos[0], //location of clipping plane
                         gluvv.clip.pos[1],
                         gluvv.clip.pos[2]);
            gl.glMultMatrixf(gluvv.clip.xform, 0); //rotation of clip plane
            double[] zup = {0, 0, -1, 0}; // always in z direction  // zup[4]
            gl.glEnable(GL.GL_CLIP_PLANE5); //enable the gl clip plane
            DoubleBuffer dBuff = DoubleBuffer.wrap(zup);
            dBuff.rewind();
            gl.glClipPlane(GL.GL_CLIP_PLANE5, dBuff);
          }
          gl.glPopMatrix();
      }
    }

    if ( (gluvv.clip.on == 0) || (gluvv.clip.ortho == 0))return;

    Volume v = gluvv.mv.volumes[vol];
    float[][] c = new float[4][3];

    math.copyV3(c[0], gluvv.clip.corners[0]);
    math.copyV3(c[1], gluvv.clip.corners[1]);
    math.copyV3(c[2], gluvv.clip.corners[2]);
    math.copyV3(c[3], gluvv.clip.corners[3]);
    //this moves the clip plane to sub-volume space
    c[0][0] = (float)math.CLAMP_ARB(0, c[0][0] - v.xfPos, v.xfSize);
    c[1][0] = (float)math.CLAMP_ARB(0, c[1][0] - v.xfPos, v.xfSize);
    c[2][0] = (float)math.CLAMP_ARB(0, c[2][0] - v.xfPos, v.xfSize);
    c[3][0] = (float)math.CLAMP_ARB(0, c[3][0] - v.xfPos, v.xfSize);
    c[0][1] = (float)math.CLAMP_ARB(0, c[0][1] - v.yfPos, v.yfSize);
    c[1][1] = (float)math.CLAMP_ARB(0, c[1][1] - v.yfPos, v.yfSize);
    c[2][1] = (float)math.CLAMP_ARB(0, c[2][1] - v.yfPos, v.yfSize);
    c[3][1] = (float)math.CLAMP_ARB(0, c[3][1] - v.yfPos, v.yfSize);
    c[0][2] = (float)math.CLAMP_ARB(0, c[0][2] - v.zfPos, v.zfSize);
    c[1][2] = (float)math.CLAMP_ARB(0, c[1][2] - v.zfPos, v.zfSize);
    c[2][2] = (float)math.CLAMP_ARB(0, c[2][2] - v.zfPos, v.zfSize);
    c[3][2] = (float)math.CLAMP_ARB(0, c[3][2] - v.zfPos, v.zfSize);

    gl.glEnable(GL.GL_REGISTER_COMBINERS_NV);
    gl.glCombinerParameteriNV(GL.GL_NUM_GENERAL_COMBINERS_NV, 2);

    // I am just using these to replace the texture's alpha with a different one
    float[] alpha = {1, 0, 0, gluvv.clip.alpha};   // alpha[4]
    gl.glCombinerParameterfvNV(GL.GL_CONSTANT_COLOR0_NV, alpha, 0); //set W
    float[] greenish = {0, 1, 0, 1};  // greenish[4]
    gl.glCombinerParameterfvNV(GL.GL_CONSTANT_COLOR1_NV, greenish, 0); //set W

    //blue is the primary color
    gl.glColor4f(0, 0, 1, 1);

    //this is not used right now
    //now for some painfull swizling
    //extract the red, extract the green
    gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_RGB, GL.GL_VARIABLE_A_NV, GL.GL_TEXTURE0, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
    gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_RGB, GL.GL_VARIABLE_B_NV, GL.GL_CONSTANT_COLOR0_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
    gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_RGB, GL.GL_VARIABLE_C_NV, GL.GL_TEXTURE0, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
    gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_RGB, GL.GL_VARIABLE_D_NV, GL.GL_CONSTANT_COLOR1_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
    gl.glCombinerOutputNV(GL.GL_COMBINER0_NV, GL.GL_RGB,
                            GL.GL_SPARE0_NV, GL.GL_SPARE1_NV, GL.GL_DISCARD_NV, GL.GL_NONE, GL.GL_NONE, true, true, false);
    System.err.println("nv20volren" + " gcom0.4");

    //alpha goes to red + green
    gl.glCombinerInputNV(GL.GL_COMBINER1_NV, GL.GL_RGB, GL.GL_VARIABLE_A_NV, GL.GL_TEXTURE0, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
    gl.glCombinerInputNV(GL.GL_COMBINER1_NV, GL.GL_RGB, GL.GL_VARIABLE_B_NV, GL.GL_CONSTANT_COLOR0_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
    gl.glCombinerInputNV(GL.GL_COMBINER1_NV, GL.GL_RGB, GL.GL_VARIABLE_C_NV, GL.GL_SPARE0_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
    gl.glCombinerInputNV(GL.GL_COMBINER1_NV, GL.GL_RGB, GL.GL_VARIABLE_D_NV, GL.GL_CONSTANT_COLOR1_NV, GL.GL_UNSIGNED_INVERT_NV, GL.GL_RGB);
    gl.glCombinerOutputNV(GL.GL_COMBINER1_NV, GL.GL_RGB,
                            GL.GL_DISCARD_NV, GL.GL_DISCARD_NV, GL.GL_TEXTURE0, GL.GL_NONE, GL.GL_NONE, false, false, false);
    System.err.println("nv20volren" + " gcom1.4");

    //add blue now, set the correct alpha
    gl.glFinalCombinerInputNV(GL.GL_VARIABLE_A_NV, GL.GL_SPARE1_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
    System.err.println("nv20volren" + " fcom1");
    gl.glFinalCombinerInputNV(GL.GL_VARIABLE_B_NV, GL.GL_PRIMARY_COLOR_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
    System.err.println("nv20volren" + " fcom2");
    gl.glFinalCombinerInputNV(GL.GL_VARIABLE_C_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
    System.err.println("nv20volren" + " fcom3");
    gl.glFinalCombinerInputNV(GL.GL_VARIABLE_D_NV, GL.GL_TEXTURE0, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
    System.err.println("nv20volren" + " fcom0");

    gl.glFinalCombinerInputNV(GL.GL_VARIABLE_G_NV, GL.GL_CONSTANT_COLOR0_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);

    gl.glEnable(GL.GL_BLEND);

    float offset = .001f; //this is used to avoid z-compete when the slice is on top

    gl.glPushMatrix();
    {
      //make sure we draw this in sub-volume space
      gl.glTranslatef(v.xfPos, v.yfPos, v.zfPos);
      if ( gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisXPos ) {
          if ( (dv < 0) && (c[0][0] > 0) && (c[0][0] < v.xfSize)) {
            float[] ov = {offset, 0, 0}; //offset vector, ov[3]
            for (int i = 0; i < 4; ++i)
              math.addV3(c[i], c[i], ov);
            renderSlice(drawable, c, vol);
          }
      } else if ( gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisXNeg ) {
          if (dv > 0) {
            float[] ov = {-offset, 0, 0}; //offset vector, ov[3]
            for (int i = 0; i < 4; ++i)
              math.addV3(c[i], c[i], ov);
            renderSlice(drawable,c, vol);
          }
      } else if ( gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisYPos ) {
          if ( (dv > 0) && (c[0][1] > 0) && (c[0][1] < v.yfSize)) {
            float[] ov = {0, offset, 0}; //offset vector, ov[3]
            for (int i = 0; i < 4; ++i)
              math.addV3(c[i], c[i], ov);
            renderSlice(drawable,c, vol);
          }
      } else if ( gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisYNeg ) {
          if ( (dv < 0) && (c[0][1] > 0) && (c[0][1] < v.yfSize)) {
            float[] ov = {0, -offset, 0}; //offset vector, ov[3]
            for (int i = 0; i < 4; ++i)
              math.addV3(c[i], c[i], ov);
            renderSlice(drawable,c, vol);
          }
      } else if ( gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisZPos ) {
          if ( (dv < 0) && (c[0][2] > 0) && (c[0][2] < v.zfSize)) {
            float[] ov = {0, 0, offset}; //offset vector, ov[3]
            for (int i = 0; i < 4; ++i)
              math.addV3(c[i], c[i], ov);
            renderSlice(drawable,c, vol);
          }
      } else if (  gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisZNeg ) {
          if ( (dv > 0) && (c[0][2] > 0) && (c[0][2] < v.zfSize)) {
            float[] ov = {0, 0, -offset}; //offset vector, ov[3]
            for (int i = 0; i < 4; ++i)
              math.addV3(c[i], c[i], ov);
            renderSlice(drawable,c, vol);
          }
      }
    }
    gl.glPopMatrix();
    gl.glDisable(GL.GL_REGISTER_COMBINERS_NV); //clean up opengl state
    gl.glActiveTexture(GL.GL_TEXTURE0);
    {
      gl.glDisable(GL.GL_TEXTURE_2D);
      gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_NONE);
      gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_TEXTURE_2D);
    }

    gl.glDisable(GL.GL_BLEND);

  }

  //============================================== Render Slice w/ corners
//======================================================================
  private void renderSlice(GLAutoDrawable drawable, float[][] c, int vol) { // float c[4][3]
    GL gl = drawable.getGL();
    gl.glActiveTexture(GL.GL_TEXTURE0);
    { //this is for the slice
      gl.glEnable(GL.GL_TEXTURE_3D);
      gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
      gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_TEXTURE_3D);
      gl.glBindTexture(GL.GL_TEXTURE_3D, texNames[vol]);
    }

    Volume v = gluvv.mv.volumes[vol];

    gl.glBegin(GL.GL_QUADS);
    {
      gl.glMultiTexCoord3f(GL.GL_TEXTURE0, c[0][0] / (v.xfSize),
                           c[0][1] / (v.yfSize), c[0][2] / (v.zfSize));
      gl.glVertex3f(c[0][0], c[0][1], c[0][2]);
      gl.glMultiTexCoord3f(GL.GL_TEXTURE0, c[1][0] / (v.xfSize),
                           c[1][1] / (v.yfSize), c[1][2] / (v.zfSize));
      gl.glVertex3f(c[1][0], c[1][1], c[1][2]);
      gl.glMultiTexCoord3f(GL.GL_TEXTURE0, c[2][0] / (v.xfSize),
                           c[2][1] / (v.yfSize), c[2][2] / (v.zfSize));
      gl.glVertex3f(c[2][0], c[2][1], c[2][2]);
      gl.glMultiTexCoord3f(GL.GL_TEXTURE0, c[3][0] / (v.xfSize),
                           c[3][1] / (v.yfSize), c[3][2] / (v.zfSize));
      gl.glVertex3f(c[3][0], c[3][1], c[3][2]);
    }
    gl.glEnd();

  }

  //=========================================================== resetClips
//======================================================================
  private void resetClips(GLAutoDrawable drawable)
  {
    GL gl = drawable.getGL();
    gl.glDisable(GL.GL_CLIP_PLANE0);
  }

  //======================================================== Setup Shaders
//======================================================================
  private void setupShaders(GLAutoDrawable drawable) {
    GL gl = drawable.getGL();
    gl.glEnable(GL.GL_TEXTURE_SHADER_NV);
    {
      System.err.println("nv20volren" +  " enable shader");

      gl.glActiveTexture(GL.GL_TEXTURE0);
      { //this is for the slice
        gl.glEnable(GL.GL_TEXTURE_3D);
        gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_NONE);
        gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_TEXTURE_3D);
      }
      gl.glActiveTexture(GL.GL_TEXTURE1);
      { //this is for the transfer function
        gl.glEnable(GL.GL_TEXTURE_2D);
        gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
        gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV,
                  GL.GL_DEPENDENT_AR_TEXTURE_2D_NV);
        gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_PREVIOUS_TEXTURE_INPUT_NV,
                  GL.GL_TEXTURE0);
        gl.glBindTexture(GL.GL_TEXTURE_2D, deptexName[0]);
      }
      gl.glActiveTexture(GL.GL_TEXTURE2);
      { // for shading or clipping (not both)
        if ( (gluvv.shade == gluvvShade.gluvvShadeDiff) || (gluvv.shade == gluvvShade.gluvvShadeDSpec)) {
          //shading
          gl.glEnable(GL.GL_TEXTURE_3D);
          gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
          gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_TEXTURE_3D);
          //glBindTexture(GL_TEXTURE_3D, shadeNames[0]);
        } else if(gluvv.light.shadow == 1){ //shadows w/no shading
          gl.glEnable(GL.GL_TEXTURE_2D);
          gl.glBindTexture(GL.GL_TEXTURE_2D, shadowName[0]);
          gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
          gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_TEXTURE_2D);
        } else {
          //no shading
          gl.glDisable(GL.GL_TEXTURE_2D);
          gl.glDisable(GL.GL_TEXTURE_3D);
          gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_NONE);
          gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_NONE);
        }
      }
      gl.glActiveTexture(GL.GL_TEXTURE3);
      { //3rd & 4th axis, or clipping
        if ( (gluvv.shade == gluvvShade.gluvvShadeDiff) || (gluvv.shade == gluvvShade.gluvvShadeDSpec)) { //we have to use this for shadows

          gl.glEnable(GL.GL_TEXTURE_2D);
          gl.glBindTexture(GL.GL_TEXTURE_2D, shadowName[0]);
          gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
          gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_TEXTURE_2D);
        }
        else {

          if (gluvv.dmode == gluvvDataMode.GDM_VGH || //hey! we use this for 3rd & 4th axies
              gluvv.dmode == gluvvDataMode.GDM_V1GH ||
              gluvv.dmode == gluvvDataMode.GDM_V2G ||
              gluvv.dmode == gluvvDataMode.GDM_V2GH ||
              gluvv.dmode == gluvvDataMode.GDM_V3 ||
              gluvv.dmode == gluvvDataMode.GDM_V3G ||
              gluvv.dmode == gluvvDataMode.GDM_V4) {
            gl.glEnable(GL.GL_TEXTURE_2D);
            gl.glTexEnvi(GL.GL_TEXTURE_ENV,
                         GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
            gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV,
                         GL.GL_SHADER_OPERATION_NV,
                         GL.GL_DEPENDENT_GB_TEXTURE_2D_NV);
            gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV,
                         GL.GL_PREVIOUS_TEXTURE_INPUT_NV,
                         GL.GL_TEXTURE0);
            gl.glBindTexture(GL.GL_TEXTURE_2D, deptex2Name[0]);
          }
          else if (gluvv.dmode == gluvvDataMode.GDM_V1 ||
                   gluvv.dmode == gluvvDataMode.GDM_V1G ||
                   gluvv.dmode == gluvvDataMode.GDM_V2 ||
                   gluvv.dmode == gluvvDataMode.GDM_VGH_VG ||
                   gluvv.dmode == gluvvDataMode.GDM_VGH_V) {
            //   if (gluvv.clip.ortho == 1) ;//we want to take care of this elsewhere
          }
          else {
            gl.glDisable(GL.GL_TEXTURE_2D);
            gl.glDisable(GL.GL_TEXTURE_3D);
            gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV,
                         GL.GL_SHADER_OPERATION_NV, GL.GL_NONE);
            gl.glTexEnvi(GL.GL_TEXTURE_ENV,
                         GL.GL_TEXTURE_ENV_MODE, GL.GL_NONE);
          }
        }
      }
      System.err.println("set up shader" +  " after texture3");
    }
  }

  private void resetShaders(GLAutoDrawable drawable) {
    GL gl = drawable.getGL();
    gl.glActiveTexture(GL.GL_TEXTURE3);
    { //not used
      gl.glDisable(GL.GL_TEXTURE_2D);
      gl.glDisable(GL.GL_TEXTURE_3D);
      gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_NONE);
      gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_NONE);
    }
    gl.glActiveTexture(GL.GL_TEXTURE2);
    { //not used
      gl.glDisable(GL.GL_TEXTURE_2D);
      gl.glDisable(GL.GL_TEXTURE_3D);
      gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_NONE);
      gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_NONE);
    }
    gl.glActiveTexture(GL.GL_TEXTURE1);
    { //not used
      gl.glDisable(GL.GL_TEXTURE_2D);
      gl.glDisable(GL.GL_TEXTURE_3D);
      gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_NONE);
      gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_NONE);
    }
    gl.glActiveTexture(GL.GL_TEXTURE0);
    { //not used
      gl.glDisable(GL.GL_TEXTURE_2D);
      gl.glDisable(GL.GL_TEXTURE_3D);
      gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_NONE);
      gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
    }
    gl.glDisable(GL.GL_TEXTURE_SHADER_NV);

  }

  //======================================================= Setup Reg Comb
//======================================================================

  private void setupRegComb(GLAutoDrawable drawable, float ftb)
  {
    GL gl = drawable.getGL();
          if((gluvv.shade == gluvvShade.gluvvShadeDiff) || (gluvv.shade == gluvvShade.gluvvShadeDSpec)){
                  float[] vdir = new float[3];
                  math.subV3(vdir, gluvv.env.eye, gluvv.env.at);
                  math.normalizeV3(vdir);
                  float[] ltdir = new float[3];
                  math.subV3(ltdir, gluvv.light.pos, gluvv.env.at);
                  math.normalizeV3(ltdir);
                  float[] ltoe = new float[3];
                  math.subV3(ltoe,vdir,ltdir);
                  math.scaleV3(.5f, ltoe);
                  float[] half = new float[4];
                  math.addV3(half,ltdir, ltoe);
                  half[3] = 1f;
                  float[] vhalf = new float[4];
                  float[] invx = new float[16];
                  math.inverseMatrix(invx, gluvv.rinfo.xform);
                  math.translateV3(vhalf,invx,half);
                  math.negateV3(vhalf);
                  math.normalizeV3(vhalf);
                  vhalf[0] = vhalf[0]/2 + .5f;
                  vhalf[1] = vhalf[1]/2 + .5f;
                  vhalf[2] = vhalf[2]/2 + .5f;
                  gl.glFogfv(GL.GL_FOG_COLOR, vhalf, 0);

                  float[] lpos = new float[4];
                  math.translateV3W(lpos, invx, ltdir);
                  math.negateV3(lpos);
                  math.normalizeV3(ltdir);
                  lpos[0] = lpos[0]/2f + .5f;
                  lpos[1] = lpos[1]/2f + .5f;
                  lpos[2] = lpos[2]/2f + .5f;
                  lpos[3] = 1.0f - gluvv.light.amb;
                  gl.glCombinerParameterfvNV(GL.GL_CONSTANT_COLOR1_NV, lpos, 0); //set light direction

                  gl.glEnable(GL.GL_REGISTER_COMBINERS_NV);

                  gl.glCombinerParameteriNV(GL.GL_NUM_GENERAL_COMBINERS_NV, 8);

                  //diffuse dot product (2 sided)
                  gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_RGB, GL.GL_VARIABLE_A_NV, GL.GL_TEXTURE2, GL.GL_EXPAND_NORMAL_NV, GL.GL_RGB);
                  gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_RGB, GL.GL_VARIABLE_B_NV, GL.GL_CONSTANT_COLOR1_NV, GL.GL_EXPAND_NORMAL_NV, GL.GL_RGB);
                  gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_RGB, GL.GL_VARIABLE_C_NV, GL.GL_TEXTURE2, GL.GL_EXPAND_NORMAL_NV, GL.GL_RGB);
                  gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_RGB, GL.GL_VARIABLE_D_NV, GL.GL_CONSTANT_COLOR1_NV, GL.GL_EXPAND_NEGATE_NV, GL.GL_RGB);
                  gl.glCombinerOutputNV(GL.GL_COMBINER0_NV, GL.GL_RGB,
                          GL.GL_SPARE0_NV, GL.GL_SPARE1_NV, GL.GL_DISCARD_NV, GL.GL_NONE, GL.GL_NONE, true, true, false);
                  System.err.println("nv20volren" + " gcom0.4");

                  //compute diffuse contribution (sum of diff-dot times color)
                  gl.glCombinerInputNV(GL.GL_COMBINER1_NV, GL.GL_RGB, GL.GL_VARIABLE_A_NV, GL.GL_SPARE1_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                  gl.glCombinerInputNV(GL.GL_COMBINER1_NV, GL.GL_RGB, GL.GL_VARIABLE_B_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                  gl.glCombinerInputNV(GL.GL_COMBINER1_NV, GL.GL_RGB, GL.GL_VARIABLE_C_NV, GL.GL_SPARE0_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                  gl.glCombinerInputNV(GL.GL_COMBINER1_NV, GL.GL_RGB, GL.GL_VARIABLE_D_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                  gl.glCombinerOutputNV(GL.GL_COMBINER1_NV, GL.GL_RGB,
                          GL.GL_DISCARD_NV, GL.GL_DISCARD_NV, GL.GL_SPARE0_NV, GL.GL_NONE, GL.GL_NONE, false, false, false);
                  System.err.println("nv20volren" + " gcom1.4");

                  //add diffuse and ambient (add in ambient and square the diffuse)
                  gl.glCombinerInputNV(GL.GL_COMBINER2_NV, GL.GL_RGB, GL.GL_VARIABLE_A_NV, GL.GL_SPARE0_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                  gl.glCombinerInputNV(GL.GL_COMBINER2_NV, GL.GL_RGB, GL.GL_VARIABLE_B_NV, GL.GL_SPARE0_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                  gl.glCombinerInputNV(GL.GL_COMBINER2_NV, GL.GL_RGB, GL.GL_VARIABLE_C_NV, GL.GL_CONSTANT_COLOR1_NV, GL.GL_UNSIGNED_INVERT_NV, GL.GL_ALPHA);
                  gl.glCombinerInputNV(GL.GL_COMBINER2_NV, GL.GL_RGB, GL.GL_VARIABLE_D_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                  gl.glCombinerOutputNV(GL.GL_COMBINER2_NV, GL.GL_RGB,
                          GL.GL_DISCARD_NV, GL.GL_DISCARD_NV, GL.GL_TEXTURE1, GL.GL_NONE, GL.GL_NONE, false, false, false);
                  System.err.println("nv20volren" + " gcom2.4");

                  //specular dod product (2 sided)
                  gl.glCombinerInputNV(GL.GL_COMBINER3_NV, GL.GL_RGB, GL.GL_VARIABLE_A_NV, GL.GL_TEXTURE2, GL.GL_EXPAND_NORMAL_NV, GL.GL_RGB);
                  gl.glCombinerInputNV(GL.GL_COMBINER3_NV, GL.GL_RGB, GL.GL_VARIABLE_B_NV, GL.GL_FOG, GL.GL_EXPAND_NORMAL_NV, GL.GL_RGB);
                  gl.glCombinerInputNV(GL.GL_COMBINER3_NV, GL.GL_RGB, GL.GL_VARIABLE_C_NV, GL.GL_TEXTURE2, GL.GL_EXPAND_NEGATE_NV, GL.GL_RGB);
                  gl.glCombinerInputNV(GL.GL_COMBINER3_NV, GL.GL_RGB, GL.GL_VARIABLE_D_NV, GL.GL_FOG, GL.GL_EXPAND_NORMAL_NV, GL.GL_RGB);
                  gl.glCombinerOutputNV(GL.GL_COMBINER3_NV, GL.GL_RGB,
                          GL.GL_SPARE1_NV, GL.GL_TEXTURE2, GL.GL_DISCARD_NV, GL.GL_NONE, GL.GL_NONE, true, true, false);
                  System.err.println("nv20volren" + " gcom3.4");

                  //specular 2nd power & sum to one value
                  gl.glCombinerInputNV(GL.GL_COMBINER4_NV, GL.GL_RGB, GL.GL_VARIABLE_A_NV, GL.GL_SPARE1_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                  gl.glCombinerInputNV(GL.GL_COMBINER4_NV, GL.GL_RGB, GL.GL_VARIABLE_B_NV, GL.GL_SPARE1_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                  gl.glCombinerInputNV(GL.GL_COMBINER4_NV, GL.GL_RGB, GL.GL_VARIABLE_C_NV, GL.GL_TEXTURE2, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                  gl.glCombinerInputNV(GL.GL_COMBINER4_NV, GL.GL_RGB, GL.GL_VARIABLE_D_NV, GL.GL_TEXTURE2, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                  gl.glCombinerOutputNV(GL.GL_COMBINER4_NV, GL.GL_RGB,
                          GL.GL_TEXTURE2, GL.GL_DISCARD_NV, GL.GL_DISCARD_NV, GL.GL_NONE, GL.GL_NONE, false, false, false);
                  System.err.println("nv20volren" + " gcom4.4");

                  //shadow weight, specular 4th power
                  gl.glCombinerInputNV(GL.GL_COMBINER5_NV, GL.GL_RGB, GL.GL_VARIABLE_A_NV, GL.GL_TEXTURE3, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                  gl.glCombinerInputNV(GL.GL_COMBINER5_NV, GL.GL_RGB, GL.GL_VARIABLE_B_NV, GL.GL_CONSTANT_COLOR1_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                  gl.glCombinerInputNV(GL.GL_COMBINER5_NV, GL.GL_RGB, GL.GL_VARIABLE_C_NV, GL.GL_TEXTURE2, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                  gl.glCombinerInputNV(GL.GL_COMBINER5_NV, GL.GL_RGB, GL.GL_VARIABLE_D_NV, GL.GL_TEXTURE2, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                  gl.glCombinerOutputNV(GL.GL_COMBINER5_NV, GL.GL_RGB,
                          GL.GL_TEXTURE3, GL.GL_TEXTURE2, GL.GL_DISCARD_NV, GL.GL_NONE, GL.GL_NONE, false, false, false);
                  System.err.println("nv20volren" + " gcom5.4");

                  //shadow diffuse & specular 8th power
                  gl.glCombinerInputNV(GL.GL_COMBINER6_NV, GL.GL_RGB, GL.GL_VARIABLE_A_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                  if(gluvv.light.shadow == 1)
                          gl.glCombinerInputNV(GL.GL_COMBINER6_NV, GL.GL_RGB, GL.GL_VARIABLE_B_NV, GL.GL_TEXTURE3, GL.GL_UNSIGNED_INVERT_NV, GL.GL_ALPHA);
                  else
                          gl.glCombinerInputNV(GL.GL_COMBINER6_NV, GL.GL_RGB, GL.GL_VARIABLE_B_NV, GL.GL_ZERO, GL.GL_UNSIGNED_INVERT_NV, GL.GL_ALPHA);
                  gl.glCombinerInputNV(GL.GL_COMBINER6_NV, GL.GL_RGB, GL.GL_VARIABLE_C_NV, GL.GL_TEXTURE2, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                  gl.glCombinerInputNV(GL.GL_COMBINER6_NV, GL.GL_RGB, GL.GL_VARIABLE_D_NV, GL.GL_TEXTURE2, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                  gl.glCombinerOutputNV(GL.GL_COMBINER6_NV, GL.GL_RGB,
                          GL.GL_TEXTURE1, GL.GL_TEXTURE2, GL.GL_DISCARD_NV, GL.GL_NONE, GL.GL_NONE, false, false, false);
                  System.err.println("nv20volren" + " gcom6.4");

                  //specular shadow add to diffuse
                  gl.glCombinerInputNV(GL.GL_COMBINER7_NV, GL.GL_RGB, GL.GL_VARIABLE_A_NV, GL.GL_SPARE1_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                  if(gluvv.light.shadow == 1)
                          gl.glCombinerInputNV(GL.GL_COMBINER7_NV, GL.GL_RGB, GL.GL_VARIABLE_B_NV, GL.GL_TEXTURE3, GL.GL_UNSIGNED_INVERT_NV, GL.GL_ALPHA);
                  else
                          gl.glCombinerInputNV(GL.GL_COMBINER7_NV, GL.GL_RGB, GL.GL_VARIABLE_B_NV, GL.GL_ZERO, GL.GL_UNSIGNED_INVERT_NV, GL.GL_RGB);
                  gl.glCombinerInputNV(GL.GL_COMBINER7_NV, GL.GL_RGB, GL.GL_VARIABLE_C_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                  gl.glCombinerInputNV(GL.GL_COMBINER7_NV, GL.GL_RGB, GL.GL_VARIABLE_D_NV, GL.GL_ZERO, GL.GL_UNSIGNED_INVERT_NV, GL.GL_RGB);
                  gl.glCombinerOutputNV(GL.GL_COMBINER7_NV, GL.GL_RGB,
                          GL.GL_DISCARD_NV, GL.GL_TEXTURE2, GL.GL_TEXTURE1, GL.GL_NONE, GL.GL_NONE, false, false, false);
                  System.err.println("nv20volren" + " gcom7.4");

                  //final combiner
                  gl.glFinalCombinerInputNV(GL.GL_VARIABLE_A_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                  if(ftb <= 0)
                          gl.glFinalCombinerInputNV(GL.GL_VARIABLE_B_NV, GL.GL_ZERO, GL.GL_UNSIGNED_INVERT_NV, GL.GL_RGB);
                  else
                          gl.glFinalCombinerInputNV(GL.GL_VARIABLE_B_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                  gl.glFinalCombinerInputNV(GL.GL_VARIABLE_C_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                  //gl.glFinalCombinerInputNV(GL.GL_VARIABLE_D_NV, GL.GL_TEXTURE2, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                  gl.glFinalCombinerInputNV(GL.GL_VARIABLE_D_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                  gl.glFinalCombinerInputNV(GL.GL_VARIABLE_E_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                  gl.glFinalCombinerInputNV(GL.GL_VARIABLE_F_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                  gl.glFinalCombinerInputNV(GL.GL_VARIABLE_G_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                  System.err.println("nv20volren" + " fcom.0");
          }
          else{ //no shading =================================================

                  float[] amb = {1,1,1,1.0f - gluvv.light.amb}; //amb[4], ambiant shadow factor

                  if ( gluvv.dmode == gluvvDataMode.GDM_VGH ||
                       gluvv.dmode == gluvvDataMode.GDM_V1GH ||
                       gluvv.dmode == gluvvDataMode.GDM_V2G ||
                       gluvv.dmode == gluvvDataMode.GDM_V2GH ||
                       gluvv.dmode == gluvvDataMode.GDM_V3 ||
                       gluvv.dmode == gluvvDataMode.GDM_V3G ||
                       gluvv.dmode == gluvvDataMode.GDM_V4 ) {
                          gl.glEnable(GL.GL_REGISTER_COMBINERS_NV);

                          gl.glCombinerParameterfvNV(GL.GL_CONSTANT_COLOR1_NV, amb, 0); //set ambiant contribution

                          gl.glCombinerParameteriNV(GL.GL_NUM_GENERAL_COMBINERS_NV, 2);

                          gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_ALPHA, GL.GL_VARIABLE_A_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                          gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_ALPHA, GL.GL_VARIABLE_B_NV, GL.GL_TEXTURE3, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                          gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_ALPHA, GL.GL_VARIABLE_C_NV, GL.GL_TEXTURE2, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                          gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_ALPHA, GL.GL_VARIABLE_D_NV, GL.GL_CONSTANT_COLOR1_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                          gl.glCombinerOutputNV(GL.GL_COMBINER0_NV, GL.GL_ALPHA,
                                  GL.GL_TEXTURE1, GL.GL_TEXTURE2, GL.GL_DISCARD_NV, GL.GL_NONE, GL.GL_NONE, false, false, false);
                          System.err.println("nv20volren" + " gcom7.4");

                          if(gluvv.light.shadow == 1){
                                  gl.glCombinerInputNV(GL.GL_COMBINER1_NV, GL.GL_RGB, GL.GL_VARIABLE_A_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                                  gl.glCombinerInputNV(GL.GL_COMBINER1_NV, GL.GL_RGB, GL.GL_VARIABLE_B_NV, GL.GL_TEXTURE2, GL.GL_UNSIGNED_INVERT_NV, GL.GL_ALPHA);
                                  gl.glCombinerInputNV(GL.GL_COMBINER1_NV, GL.GL_RGB, GL.GL_VARIABLE_C_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                                  gl.glCombinerInputNV(GL.GL_COMBINER1_NV, GL.GL_RGB, GL.GL_VARIABLE_D_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                                  gl.glCombinerOutputNV(GL.GL_COMBINER1_NV, GL.GL_RGB,
                                          GL.GL_TEXTURE1, GL.GL_DISCARD_NV, GL.GL_DISCARD_NV, GL.GL_NONE, GL.GL_NONE, false, false, false);
                                  System.err.println("nv20volren" + " gcom7.4");
                          } else {
                                  gl.glCombinerOutputNV(GL.GL_COMBINER1_NV, GL.GL_RGB,
                                          GL.GL_DISCARD_NV, GL.GL_DISCARD_NV, GL.GL_DISCARD_NV, GL.GL_NONE, GL.GL_NONE, false, false, false);
                          }

                          gl.glFinalCombinerInputNV(GL.GL_VARIABLE_A_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                          //gl.glFinalCombinerInputNV(GL.GL_VARIABLE_A_NV, GL.GL_TEXTURE2, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                          if(ftb <= 0)
                                  gl.glFinalCombinerInputNV(GL.GL_VARIABLE_B_NV, GL.GL_ZERO, GL.GL_UNSIGNED_INVERT_NV, GL.GL_RGB);
                          else
                                  gl.glFinalCombinerInputNV(GL.GL_VARIABLE_B_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                          gl.glFinalCombinerInputNV(GL.GL_VARIABLE_C_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                          gl.glFinalCombinerInputNV(GL.GL_VARIABLE_D_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                          gl.glFinalCombinerInputNV(GL.GL_VARIABLE_E_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                          gl.glFinalCombinerInputNV(GL.GL_VARIABLE_F_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                          gl.glFinalCombinerInputNV(GL.GL_VARIABLE_G_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                          System.err.println("nv20volren" + " fcom.0");
                  } else {
                          gl.glEnable(GL.GL_REGISTER_COMBINERS_NV);

                          gl.glCombinerParameterfvNV(GL.GL_CONSTANT_COLOR1_NV, amb, 0); //set ambiant contribution

                          gl.glCombinerParameteriNV(GL.GL_NUM_GENERAL_COMBINERS_NV, 2);

                          gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_ALPHA, GL.GL_VARIABLE_A_NV, GL.GL_TEXTURE2, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                          gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_ALPHA, GL.GL_VARIABLE_B_NV, GL.GL_CONSTANT_COLOR1_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                          gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_ALPHA, GL.GL_VARIABLE_C_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                          gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_ALPHA, GL.GL_VARIABLE_D_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                          gl.glCombinerOutputNV(GL.GL_COMBINER0_NV, GL.GL_ALPHA,
                                  GL.GL_TEXTURE2, GL.GL_DISCARD_NV, GL.GL_DISCARD_NV, GL.GL_NONE, GL.GL_NONE, false, false, false);
                          System.err.println("nv20volren" + "gcom7.4 ");

                          if(gluvv.light.shadow == 1){
                                  gl.glCombinerInputNV(GL.GL_COMBINER1_NV, GL.GL_RGB, GL.GL_VARIABLE_A_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                                  gl.glCombinerInputNV(GL.GL_COMBINER1_NV, GL.GL_RGB, GL.GL_VARIABLE_B_NV, GL.GL_TEXTURE2, GL.GL_UNSIGNED_INVERT_NV, GL.GL_ALPHA);
                                  gl.glCombinerInputNV(GL.GL_COMBINER1_NV, GL.GL_RGB, GL.GL_VARIABLE_C_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                                  gl.glCombinerInputNV(GL.GL_COMBINER1_NV, GL.GL_RGB, GL.GL_VARIABLE_D_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                                  gl.glCombinerOutputNV(GL.GL_COMBINER1_NV, GL.GL_RGB,
                                          GL.GL_TEXTURE1, GL.GL_DISCARD_NV, GL.GL_DISCARD_NV, GL.GL_NONE, GL.GL_NONE, false, false, false);
                                  System.err.println("nv20volren " + " gcom7.4");
                          } else {
                                  gl.glCombinerOutputNV(GL.GL_COMBINER1_NV, GL.GL_RGB,
                                          GL.GL_DISCARD_NV, GL.GL_DISCARD_NV, GL.GL_DISCARD_NV, GL.GL_NONE, GL.GL_NONE, false, false, false);
                          }

                          gl.glFinalCombinerInputNV(GL.GL_VARIABLE_A_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                          if(ftb <= 0)
                                  gl.glFinalCombinerInputNV(GL.GL_VARIABLE_B_NV, GL.GL_ZERO, GL.GL_UNSIGNED_INVERT_NV, GL.GL_RGB);
                          else
                                  gl.glFinalCombinerInputNV(GL.GL_VARIABLE_B_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                          gl.glFinalCombinerInputNV(GL.GL_VARIABLE_C_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                          gl.glFinalCombinerInputNV(GL.GL_VARIABLE_D_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                          gl.glFinalCombinerInputNV(GL.GL_VARIABLE_E_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                          gl.glFinalCombinerInputNV(GL.GL_VARIABLE_F_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                          gl.glFinalCombinerInputNV(GL.GL_VARIABLE_G_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                          System.err.println("nv20volren" + " fcom.0");

                  }
          }

  }


  private void resetRegComb(GLAutoDrawable drawable)
  {
    GL gl = drawable.getGL();
    gl.glDisable(GL.GL_REGISTER_COMBINERS_NV);
  }


  //====================================================== Set Up P-Buffer
//======================================================================
  private void setupPBuff(GLAutoDrawable drawable)       //prepare pbuffer for rendering
  {
    GL gl = drawable.getGL();
          if(gluvv.picking == 1) return;

          // pbuff.MakeCurrent();

          System.err.println("volren" + " setupPbub()-start");
          //compute the view-port for this projection
          float[] winsz = {.8f, .8f, .5f};
          float[] winszp = new float[3];
          math.translateV3WD(winszp, gluvv.light.pj,winsz);
          xbias = winszp[0];
          ybias = winszp[1];
          xscale = gluvv.light.buffsz[0]/(winszp[0]*2);
          yscale = gluvv.light.buffsz[1]/(winszp[1]*2);

  /*
          glMatrixMode( GL_PROJECTION );{  //figure out the window size
                          glLoadIdentity();
                          glOrtho(0,gluvv.light.buffsz[0],0,gluvv.light.buffsz[1],1,3);
          }
          glMatrixMode(GL_MODELVIEW);{
                          glLoadIdentity();
                          gluLookAt(0,0,2,
                                                                  0,0,0,
                                                                  0,1,0);
          }
  */

          gl.glEnable(GL.GL_BLEND);
          gl.glBlendFunc(GL.GL_ONE, GL.GL_ONE_MINUS_SRC_ALPHA);
          gl.glDisable(GL.GL_LIGHTING);
          gl.glDisable(GL.GL_DEPTH_TEST);
          gl.glClear(GL.GL_COLOR_BUFFER_BIT);



          //setup shader stage
          gl.glEnable(GL.GL_TEXTURE_SHADER_NV); {
                  System.err.println("nv20volren" + " enable shader");

                  gl.glActiveTexture(GL.GL_TEXTURE0); { //this is for the slice
                          gl.glEnable(GL.GL_TEXTURE_3D);
                          gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
                          gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_TEXTURE_3D);
                  } //end texture 2
                  gl.glActiveTexture(GL.GL_TEXTURE1); { //this is for the transfer function
                          gl.glEnable(GL.GL_TEXTURE_2D);
                          gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
                          gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_DEPENDENT_AR_TEXTURE_2D_NV);
                          gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_PREVIOUS_TEXTURE_INPUT_NV, GL.GL_TEXTURE0);
                          gl.glBindTexture(GL.GL_TEXTURE_2D, deptexName[0]);
                  } //end texture 1
                  gl.glActiveTexture(GL.GL_TEXTURE2); { // shadow texture
                          gl.glEnable(GL.GL_TEXTURE_2D);
                          gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
                          gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_TEXTURE_2D);
                          gl.glBindTexture(GL.GL_TEXTURE_2D, shadowName[0]);
                  } //end texture 2
                  gl.glActiveTexture(GL.GL_TEXTURE3); { //3rd & 4th axis, or clipping
                          if ( gluvv.dmode == gluvvDataMode.GDM_VGH ||
                               gluvv.dmode == gluvvDataMode.GDM_V1GH ||
                               gluvv.dmode == gluvvDataMode.GDM_V2G ||
                               gluvv.dmode == gluvvDataMode.GDM_V2GH |
                               gluvv.dmode == gluvvDataMode.GDM_V3 ||
                               gluvv.dmode == gluvvDataMode.GDM_V3G ||
                               gluvv.dmode == gluvvDataMode.GDM_V4 ) {
                                  gl.glEnable(GL.GL_TEXTURE_2D);
                                  gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
                                  gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_DEPENDENT_GB_TEXTURE_2D_NV);
                                  gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_PREVIOUS_TEXTURE_INPUT_NV, GL.GL_TEXTURE0);
                                  gl.glBindTexture(GL.GL_TEXTURE_2D, deptex2Name[0]);
                          } else if ( gluvv.dmode ==  gluvvDataMode.GDM_V1 ||
                                      gluvv.dmode == gluvvDataMode.GDM_V1G ||
                                      gluvv.dmode == gluvvDataMode.GDM_V2 ||
                                      gluvv.dmode == gluvvDataMode.GDM_VGH_VG ||
                                      gluvv.dmode == gluvvDataMode.GDM_VGH_V ) {
                                  if( gluvv.clip.ortho == 0) ; //we want to take care of this elsewhere
                          } else {
                                  gl.glDisable(GL.GL_TEXTURE_2D);
                                  gl.glDisable(GL.GL_TEXTURE_3D);
                                  gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_NONE);
                                  gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_NONE);
                          }
                  } //end texture 3
                  System.err.println("pbuffer::set up shader" +  " after texture3");
          }

          //now setup the register combiners
          if ( gluvv.dmode == gluvvDataMode.GDM_VGH ||
               gluvv.dmode == gluvvDataMode.GDM_V1GH ||
               gluvv.dmode == gluvvDataMode.GDM_V2G ||
               gluvv.dmode == gluvvDataMode.GDM_V2GH ||
               gluvv.dmode == gluvvDataMode.GDM_V3 ||
               gluvv.dmode == gluvvDataMode.GDM_V3G ||
               gluvv.dmode == gluvvDataMode.GDM_V4 ) {
                          gl.glEnable(GL.GL_REGISTER_COMBINERS_NV);

                          gl.glCombinerParameteriNV(GL.GL_NUM_GENERAL_COMBINERS_NV, 1);
                          //compute alpha with 3rd axis
                          gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_ALPHA, GL.GL_VARIABLE_A_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                          gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_ALPHA, GL.GL_VARIABLE_B_NV, GL.GL_TEXTURE3, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                          gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_ALPHA, GL.GL_VARIABLE_C_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                          gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_ALPHA, GL.GL_VARIABLE_D_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                          gl.glCombinerOutputNV(GL.GL_COMBINER0_NV, GL.GL_ALPHA,
                                  GL.GL_TEXTURE1, GL.GL_DISCARD_NV, GL.GL_DISCARD_NV, GL.GL_NONE, GL.GL_NONE, false, false, false);
                          System.err.println("nv20volren" + " gcom7.4");
                          //composite new slice under old slice
   /*
                          gl.glCombinerInputNV(GL.GL_COMBINER1_NV, GL.GL_ALPHA, GL.GL_VARIABLE_A_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                          gl.glCombinerInputNV(GL.GL_COMBINER1_NV, GL.GL_ALPHA, GL.GL_VARIABLE_B_NV, GL.GL_TEXTURE2, GL.GL_UNSIGNED_INVERT_NV, GL.GL_ALPHA);
                          gl.glCombinerInputNV(GL.GL_COMBINER1_NV, GL.GL_ALPHA, GL.GL_VARIABLE_C_NV, GL.GL_TEXTURE2, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                          gl.glCombinerInputNV(GL.GL_COMBINER1_NV, GL.GL_ALPHA, GL.GL_VARIABLE_D_NV, GL.GL_ZERO, GL.GL_UNSIGNED_INVERT_NV, GL.GL_ALPHA);
                          gl.glCombinerOutputNV(GL.GL_COMBINER1_NV, GL.GL_ALPHA,
                                  GL.GL_DISCARD_NV, GL.GL_DISCARD_NV, GL.GL_TEXTURE1, GL.GL_NONE, GL.GL_NONE, GL.GL_FALSE, GL.GL_FALSE, GL.GL_FALSE);
                          System.err.println("nv20volren","gcom7.4");
                          //what is in the frame buffer? (always front to back)
   */

                          gl.glFinalCombinerInputNV(GL.GL_VARIABLE_A_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                    gl.glFinalCombinerInputNV(GL.GL_VARIABLE_B_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                    //gl.glFinalCombinerInputNV(GL.GL_VARIABLE_B_NV, GL.GL_ZERO, GL.GL_UNSIGNED_INVERT_NV, GL.GL_RGB);
                          gl.glFinalCombinerInputNV(GL.GL_VARIABLE_C_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                          gl.glFinalCombinerInputNV(GL.GL_VARIABLE_D_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                          gl.glFinalCombinerInputNV(GL.GL_VARIABLE_E_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                          gl.glFinalCombinerInputNV(GL.GL_VARIABLE_F_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                          gl.glFinalCombinerInputNV(GL.GL_VARIABLE_G_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                          //gl.glFinalCombinerInputNV(GL.GL_VARIABLE_G_NV, GL.GL_ZERO, GL.GL_UNSIGNED_INVERT_NV, GL.GL_ALPHA);
                          System.err.println("nv20volren" + " fcom.0");
          } else {

                          gl.glEnable(GL.GL_REGISTER_COMBINERS_NV);
                          gl.glCombinerParameteriNV(GL.GL_NUM_GENERAL_COMBINERS_NV, 1);
                          //NO OP
                          gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_ALPHA, GL.GL_VARIABLE_A_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                          gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_ALPHA, GL.GL_VARIABLE_B_NV, GL.GL_TEXTURE2, GL.GL_UNSIGNED_INVERT_NV, GL.GL_ALPHA);
                          gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_ALPHA, GL.GL_VARIABLE_C_NV, GL.GL_TEXTURE2, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                          gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_ALPHA, GL.GL_VARIABLE_D_NV, GL.GL_ZERO, GL.GL_UNSIGNED_INVERT_NV, GL.GL_ALPHA);
                          gl.glCombinerOutputNV(GL.GL_COMBINER0_NV, GL.GL_ALPHA,
                                  GL.GL_DISCARD_NV, GL.GL_DISCARD_NV, GL.GL_DISCARD_NV, GL.GL_NONE, GL.GL_NONE, false, false, false);
                          System.err.println("nv20volren" + " gcom7.4");
                          //to the frame buffer!!! (always front to back)
                          gl.glFinalCombinerInputNV(GL.GL_VARIABLE_A_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                          gl.glFinalCombinerInputNV(GL.GL_VARIABLE_B_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                          gl.glFinalCombinerInputNV(GL.GL_VARIABLE_C_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                          gl.glFinalCombinerInputNV(GL.GL_VARIABLE_D_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                          gl.glFinalCombinerInputNV(GL.GL_VARIABLE_E_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                          gl.glFinalCombinerInputNV(GL.GL_VARIABLE_F_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                          gl.glFinalCombinerInputNV(GL.GL_VARIABLE_G_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                          System.err.println("nv20volren" + " fcom.0");

          }

          System.err.println("volren" +  " end of setup pbuff");
          // MakeCurrent();

  }


  //=================================================== Render 3D Vec Algn
//======================================================================
  /**
   * GLdouble mv[16],
                  unsigned int v,
                  float vo[8][3],	  //volume vertex coords model-space coords
                  float tx[8][3],	  //texture vertex coords tex-space coords
                  float axis[4])	 //axis to slice along world-space coords
   * @param sampleFrequency float
   * @param mv GLdouble[]
   */
  private void render3DVA(GLAutoDrawable drawable,
                          float sampleFrequency,
                  double[] mv,
                  int v,
                  float[][] vo,	  //volume vertex coords model-space coords
                  float[][] tx,	  //texture vertex coords tex-space coords
                  float[] axis)	 //axis to slice along world-space coords
  {
     int i, j, k;
     GL gl = drawable.getGL();
     float[][] rv = new float[8][3];     //the rotated volume (may include a scale)
           //float lv[8][4];
     float maxval = -100; //(tmp)
     float minval = 100;
     int minvert = 0;
     double[] mvinv = new double[16];

     math.inverseMatrix(mvinv, mv); //invert model view matrix

           float[] start = new float[3];       //pick a reference point for rendering
           math.copyV3(start, axis);  // this is some distance out the rendering axis
           math.negateV3(start);
           math.scaleV3(100, start);

           //build the light transform
           float[] tm0 = new float[16];
           float[] tb = {1f,0f,0f,0f,         //translate back by a half
                         0f,1f,0f,0f,
                         0f,0f,1f,0f,
                         gluvv.mv.volumes[v].xfPos-gluvv.mv.xfSize/2f,
                         gluvv.mv.volumes[v].yfPos-gluvv.mv.yfSize/2f,
                         gluvv.mv.volumes[v].zfPos-gluvv.mv.zfSize/2f, 1f};
           math.matrixMult(tm0,gluvv.rinfo.xform,tb);
           float[] ltxf = new float[16];
           math.matrixMult(ltxf,gluvv.light.xf,tm0);

     int vnear = 0;
     int vfar  = 0;

     for(i=0; i<8; ++i){   //translate model to world coords (view space)
                   math.translateV3(rv[i], mv, vo[i]);
                   float[] tv = new float[3];
                   math.subV3(tv, start, rv[i]);       //compute vertex distance from reference
                   float tvd = math.normV3(tv);
  /*
                   translateV4_3W(lv[i], ltxf, vo[i]);
                   cerr << " " << i << " "; printV4(lv[i]); cerr << endl;
                   lv[i][0] /= lv[i][3];
                   lv[i][1] /= lv[i][3];
                   lv[i][2] /= lv[i][3];
                   lv[i][0] = (lv[i][0] + xbias)*xscale;
                   lv[i][1] = (lv[i][1] + ybias)*yscale;
  */
                   if(maxval < tvd){
                           maxval = tvd;
                           vfar = i;   //end point for slicing
                   }
                   if(minval > tvd){
                           minval = tvd;
                           vnear = i;  //starting point for slicing
                   }
     }

  /*
           gl.glPushMatrix();{
                          gl.glMatrixMode( GL_PROJECTION );
                          gl.glPushMatrix();
                                  gl.glLoadIdentity();
                                  float winsz[3] = {.7, .7, .5};
                                  float winszp[3];
                                  translateV3WD(winszp, gluvv.light.pj,winsz);
                                  //cerr << "winsz "; printV3(winszp); cerr << endl;
                                  gl.glOrtho(0,gluvv.light.buffsz[0],0,gluvv.light.buffsz[1],1,3);
                          gl.glMatrixMode(GL_MODELVIEW);
                          gl.glLoadIdentity();
                          gluLookAt(0,0,2,
                                          0,0,0,
                                                                  0,1,0);
                          gl.glColor4f(1,0,0,1);
                          gl.glBegin(GL_LINES);{
                                  gl.glVertex2fv(lv[0]);
                                  gl.glVertex2fv(lv[1]);

                                  gl.glVertex2fv(lv[2]);
                                  gl.glVertex2fv(lv[3]);

                                  gl.glVertex2fv(lv[4]);
                                  gl.glVertex2fv(lv[5]);

                                  gl.glVertex2fv(lv[6]);
                                  gl.glVertex2fv(lv[7]);
                          } gl.glEnd();

           }gl.glPopMatrix();
           gl.glMatrixMode(GL_PROJECTION);
           gl.glPopMatrix();
           gl.glMatrixMode(GL_MODELVIEW);
   */

           //find the slice plane normal (sn)
           float[] vpn = new float[3];
           vpn[0] = axis[0]; vpn[1] = axis[1]; vpn[2] = axis[2];
           float[] sn = new float[3];		    //slice plane normal
           math.translateV3(sn, mvinv, vpn); //move vpn to sn (model space);
           math.normalizeV3(sn);

     //note: sn & sp are defined in Model Space, ie the space where the volume
     // is alligned with the (x,y,z) axies

           //distance between samples
     float dis = gluvv.mv.xfSize / (gluvv.mv.xiSize * sampleFrequency);

           math.cscaleV3(start, 100, sn);  //pick a point far from the volume
           math.negateV3(start);

           //find the nearest slicing point (the closest sample)
           float[] np = new float[3];// = {vo[minvert][0], vo[minvert][1], vo[minvert][2]};
           float[] vms = new float[3];   //v minus start (this is the vector from the point to the near vertex)
           math.subV3(vms, vo[vnear], start);
           float[] vmsps = new float[3];  //v minus start project on sn
           math.cscaleV3(vmsps, math.dotV3(vms, sn), sn);
           int sts = (int)(math.normV3(vmsps)/dis) + 1; //samples to start
           math.cscaleV3(np,sts*dis,sn); //the nearest slicing point
           math.addV3(np,np,start);

           //find the farthest slicing point (same process as near point)
           float[] fp = new float[3];
           math.subV3(vms, vo[vfar], start);
           math.cscaleV3(vmsps, math.dotV3(vms, sn), sn);
           sts = (int)(math.normV3(vmsps)/dis);
           math.cscaleV3(fp,sts*dis,sn);
           math.addV3(fp,fp,start);

           float[] distv = new float[3];            //distance to sample
           math.subV3(distv, np, fp);      //needs work (fix so that off angle still has the correct #of slices)
           float dist = math.normV3(distv);
           float[] del = new float[3];// = {sn[0]*dis, sn[1]*dis, sn[2]*dis};
           math.subV3(distv, vo[vfar], vo[vnear]);
           //normalizeV3(distv);   //now figure out how to move the slice point

     int samples = (int)((dist) / dis);//(total distance to be sampled)/(sample spacing)
     math.cscaleV3(del, 1/(float)samples, distv);


           //set up opengl
     gl.glDisable(GL.GL_LIGHTING); //light makes it look bad!

     gl.glDisable(GL.GL_CULL_FACE);
     gl.glPolygonMode(GL.GL_FRONT, GL.GL_FILL);
     gl.glPolygonMode(GL.GL_BACK, GL.GL_FILL);

     //gl.glEnable(GL.GL_TEXTURE_3D_EXT);

           gl.glActiveTexture(GL.GL_TEXTURE2);
     gl.glBindTexture(GL.GL_TEXTURE_3D, shadeNames[v]);

           if(gluvv.light.shadow == 1){
                   if(gluvv.picking == 0){
                          // pbuff.MakeCurrent();             ?????????????????????????????????????????????????????????
                          {
                                  gl.glActiveTexture(GL.GL_TEXTURE0);
                                  gl.glBindTexture(GL.GL_TEXTURE_3D, texNames[v]);
                          }
                          MakeCurrent();
                          System.err.println("volren" + " setting data volume");
                   }
           }

           gl.glActiveTexture(GL.GL_TEXTURE0);
     gl.glBindTexture(GL.GL_TEXTURE_3D, texNames[v]);

           gl.glEnable(GL.GL_BLEND);
           if(axis[3] <= 0){
                   gl.glBlendFunc(GL.GL_SRC_ALPHA, GL.GL_ONE_MINUS_SRC_ALPHA);
           } else {
                   gl.glEnable(GL.GL_BLEND);
                   gl.glBlendFunc(GL.GL_ONE_MINUS_DST_ALPHA, GL.GL_ONE);
                   gl.glEnable(GL.GL_BLEND);
                   gl.glDisable(GL.GL_DEPTH_TEST);
           }

     gl.glColor4f(1, 1, 1, 1);
     System.err.println("NV20VolRen3D2" + " drawVA");


     float[][] poly = new float[6][3];   //poly[6][3],  for edge intersections
     float[][] tcoord = new float[6][3]; // tcoord[6][3], for texture intersections
     float[][] tpoly = new float[6][3];  // tpoly[6][3], for transformed edge intersections
     int edges;	       // total number of edge intersections

           float[] sp = new float[3]; //starting point for slicing (aka slice point)
           math.copyV3(sp,np);

     //highly un-optimized!!!!!!!!!
     for(i = 0 ; i < samples; ++i){ //for each slice
        //increment the slice plane point by the slice distance
        sp[0] += del[0];
        sp[1] += del[1];
        sp[2] += del[2];

        edges = 0;
        //now check each edge of the volume for intersection with..
        //the plane defined by sp & sn
        //front bottom edge
        edges += intersect(vo[0], vo[1], tx[0], tx[1], rv[0], rv[1], sp, sn,
                           poly[edges], tcoord[edges], tpoly[edges]);
        //front left edge
        edges += intersect(vo[0], vo[2], tx[0], tx[2], rv[0], rv[2], sp, sn,
                           poly[edges], tcoord[edges], tpoly[edges]);
        //front right edge
        edges += intersect(vo[1], vo[3], tx[1], tx[3], rv[1], rv[3], sp, sn,
                           poly[edges], tcoord[edges], tpoly[edges]);
        //left bottom edge
        edges += intersect(vo[4], vo[0], tx[4], tx[0], rv[4], rv[0], sp, sn,
                           poly[edges], tcoord[edges], tpoly[edges]);
        //right bottom edge
        edges += intersect(vo[1], vo[5], tx[1], tx[5], rv[1], rv[5], sp, sn,
                           poly[edges], tcoord[edges], tpoly[edges]);
        //front top edge
        edges += intersect(vo[2], vo[3], tx[2], tx[3], rv[2], rv[3], sp, sn,
                           poly[edges], tcoord[edges], tpoly[edges]);
        //back bottom edge
        edges += intersect(vo[4], vo[5], tx[4], tx[5], rv[4], rv[5], sp, sn,
                           poly[edges], tcoord[edges], tpoly[edges]);
        //back left edge
        edges += intersect(vo[4], vo[6], tx[4], tx[6], rv[4], rv[6], sp, sn,
                           poly[edges], tcoord[edges], tpoly[edges]);
        //back right edge
        edges += intersect(vo[5], vo[7], tx[5], tx[7], rv[5], rv[7], sp, sn,
                           poly[edges], tcoord[edges], tpoly[edges]);
        //back top edge
        edges += intersect(vo[6], vo[7], tx[6], tx[7], rv[6], rv[7], sp, sn,
                           poly[edges], tcoord[edges], tpoly[edges]);
        //left top edge
        edges += intersect(vo[2], vo[6], tx[2], tx[6], rv[2], rv[6], sp, sn,
                           poly[edges], tcoord[edges], tpoly[edges]);
        //right top edge
        edges += intersect(vo[3], vo[7], tx[3], tx[7], rv[3], rv[7], sp, sn,
                           poly[edges], tcoord[edges], tpoly[edges]);

  /*
        if(edges == 3){ //no sort required for triangl.gles
                                  //cerr << "doing a triangl.gle" <<endl;
                                  gl.glBegin(GL.GL_TRIANGLES);
                                  {
                                          gl.glTexCoord3fv(tcoord[0]);
                                          gl.glVertex3fv(poly[0]);
                                          gl.glTexCoord3fv(tcoord[1]);
                                          gl.glVertex3fv(poly[1]);
                                          gl.glTexCoord3fv(tcoord[2]);
                                          gl.glVertex3fv(poly[2]);
                                  }
                                  gl.glEnd();
        }
        else { //compute convex hull and sort, a little piece of magic from:
  */
                                  // B.M.E. Moret & H.D. Shapiro "P to NP" pp. 453

                                  float dx, dy, tt ,theta;
                                  float[] cen = new float[2];  //tt= TempTheta
                                  cen[0] = cen[1] = 0.0f;
                                  int next;
                                  //rather than swap 3 arrays, only one?
                                  int[] order = {0,1,2,3,4,5};

                                  // order[6] could be an extreemly inefficient way to do this
                                  for(j=0; j<edges; ++j){ //find the center of the points
                                          cen[0] += tpoly[j][0];
                                          cen[1] += tpoly[j][1];
                                  } //by averaging
                                  cen[0]/= edges;
                                  cen[1]/= edges;

                                  for(j=0; j<edges; ++j){ //for each vertex
                                          theta = -10;	       //find one with largest angle from center..
                                          next = j;
                                          for ( k= j; k<edges; ++k){
                                                  //... and check angle made between other edges
                                                  dx = tpoly[order[k]][0] - cen[0];
                                                  dy = tpoly[order[k]][1] - cen[1];
                                                  if( (dx == 0) && (dy == 0)){ //same as center?
                                                          next = k;
                                                          System.out.println("what teh ");
                                                          break; //out of this for-loop
                                                  }
                                                  tt = dy/(math.ABS(dx) + math.ABS(dy)); //else compute theta [0-4]
                                                  if( dx < 0.0 ) tt = (float)(2.0 - tt); //check quadrants 2&3
                                                  else if( dy < 0.0 ) tt = (float)(4.0 + tt); //quadrant 4
                                                  if( theta <= tt ){  //grab the max theta
                                                          next = k;
                                                          theta = tt;
                                                  }
                                          } //end for(k) angle checking
                                          // i am using 'tt' as a temp
                                          // swap polygon vertex ( is this better than another branch?)
                                          // I am not sure wich is worse: swapping 3 vectors for every edge
                                          // or: using an array to index into another array??? hmmm....
                                          //   should have payed more attention in class
                                          int tmp = order[j];
                                          order[j] = order[next];
                                          order[next] = tmp;

                                  } //end for(j) edge /angle sort
  /*
                                  glBegin(GL.GL_POLYGON); //draw slice and texture map it
                                  {
                                          for(int j=0; j< edges; ++j){
                                                  glTexCoord3fv(tcoord[order[j]]);
                                                  glVertex3fv(poly[order[j]]);
                                          }
                                  }
                                  glEnd();
  */
                                  //volSlice(edges, tcoord, poly, order);
                                  volShadow(drawable, edges,tcoord,poly,order,ltxf,texNames[v]);

                                  //glFlush();
        //}//end else compute convex hull
     }// end for(i) each slice

     gl.glDisable(GL.GL_BLEND);
     gl.glDisable(GL.GL_TEXTURE_3D);
     gl.glEnable(GL.GL_LIGHTING);
     gl.glEnable(GL.GL_DEPTH_TEST);
  }

  //============================================================ Vol Slice
//======================================================================

  private void volSlice(GLAutoDrawable drawable,
                        int edges,
                        float[][] tc, // tc[6][3]
                        float[][] pc, // pc[6][3]
                        int[] order) {  // order[6]
    GL gl = drawable.getGL();
    gl.glBegin(GL.GL_POLYGON); //draw slice and texture map it
    {
      for (int j = 0; j < edges; ++j) {
        gl.glMultiTexCoord3fv(GL.GL_TEXTURE0, tc[order[j]], 0);
        gl.glMultiTexCoord3fv(GL.GL_TEXTURE2, tc[order[j]], 0);
        //glTexCoord3fv(tc[order[j]]);
        gl.glVertex3fv(pc[order[j]], 0);
      }
    }
    gl.glEnd();
  }

  //render with shadows
  /**
   * int edges,
   float tc[6][3],
   float pc[6][3],
   int   order[6],
   float ltxf[16],
   int   vol

   * @param edges int
   * @param tc float[]
   */
  private void volShadow(GLAutoDrawable drawable,
                         int edges,
                         float tc[][],
                         float pc[][],
                         int   order[],
                         float ltxf[],
                         int   vol)
  {
          GL gl = drawable.getGL();
          float minx = gluvv.light.buffsz[0];
          float maxx = 0;
          float miny = gluvv.light.buffsz[1];
          float maxy = 0;

          float[][] lv = new float[6][4];  //light space verticies
          float[][] lc = new float[6][2];  //light buffer texture coords
          for(int i=0; i<edges; ++i){
                  //move the coords into light space
                  //first the vertices
                  int idx = order[i];
                  math.translateV4_3W(lv[idx], ltxf, pc[idx]);
                  lv[idx][0] /= lv[idx][3];
                  lv[idx][1] /= lv[idx][3];
                  //now the texture coords in screen space
                  lc[idx][0] = (lv[idx][0] + xbias)/(2*xbias);
                  lc[idx][1] = (lv[idx][1] + ybias)/(2*ybias);
                  //get the light buffer coords (screen space)
                  lv[idx][0] = lc[idx][0]*gluvv.light.buffsz[0];
                  lv[idx][1] = lc[idx][1]*gluvv.light.buffsz[1];
                  //now figure out the rectangular region to copy
                  maxx = math.MAX(maxx, lv[idx][0]);
                  minx = math.MIN(minx, lv[idx][0]);
                  maxy = math.MAX(maxy, lv[idx][1]);
                  miny = math.MIN(miny, lv[idx][1]);
          }
          //copy the current alpha from the pbuffer, and draw the latest
          // pbuff.MakeCurrent();                ???????????????????????????????????????????????
          {
                  //use render to texture here??? (probably not)
                  gl.glDisable(GL.GL_TEXTURE_SHADER_NV);
                  gl.glDisable(GL.GL_REGISTER_COMBINERS_NV);

                  gl.glActiveTexture(GL.GL_TEXTURE0);
                  gl.glEnable(GL.GL_TEXTURE_2D);
                  gl.glBindTexture(GL.GL_TEXTURE_2D, shadowName[0]);
                  //glBindTexture(GL.GL_TEXTURE_2D, gluvv.volren.deptexName);
                  //minx = MAX(minx-20, 0);
                  //miny = MAX(miny-20, 0);
                  //maxx = MIN(maxx+20, gluvv.light.buffsz[0]);
                  //maxy = MIN(maxy+20, gluvv.light.buffsz[1]);
                  maxx = math.MAX(maxx, minx);
                  maxy = math.MAX(maxy, miny);
                  minx = math.MIN(maxx, minx);
                  miny = math.MIN(maxy, miny);
                  //cerr << "min " << minx << ", " << miny << "   max " << maxx << ", " << maxy << endl;
                  //cerr << " size " << (int)(maxx-minx) << ", " << (int)(maxy-miny) << endl;
                  gl.glCopyTexSubImage2D(GL.GL_TEXTURE_2D, 0, (int)minx, (int)miny,
                                            (int)minx, (int)miny,
                                      (int)(maxx-minx), (int)(maxy-miny));
                  //glRasterPos2i(minx, miny);
                  //glCopyTexSubImage2D(GL.GL_TEXTURE_2D, 0, 0, 0, 0, 0,
                  //                    (int)(maxx-minx), (int)(maxy-miny));

                  //glClear(GL.GL_COLOR_BUFFER_BIT);
  /*
                  glBegin(GL.GL_QUADS);{
                          glVertex3f(0,0,1);
                  } glEnd():
  */

                  //glCopyTexSubImage2D(GL.GL_TEXTURE_2D, 0, 0, 0, 0, 0,
                  //                    512, 512);
                  System.err.println("volren" + " after pbuff- copy tex sub");

                  gl.glEnable(GL.GL_TEXTURE_SHADER_NV);
                  gl.glEnable(GL.GL_REGISTER_COMBINERS_NV);
                  gl.glActiveTexture(GL.GL_TEXTURE0);
                  gl.glEnable(GL.GL_TEXTURE_3D);
                  gl.glBindTexture(GL.GL_TEXTURE_3D, vol);
                  gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
                  gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_TEXTURE_3D);

                  gl.glFlush();

            //glDisable(GL.GL_TEXTURE_3D);

                  gl.glColor4f(1,1,1,1);
                  gl.glBegin(GL.GL_POLYGON); //draw slice and texture map it
                  {
                          for(int j=0; j< edges; ++j){
                                  //the incomming slice
                                  gl.glMultiTexCoord3fv(GL.GL_TEXTURE0,tc[order[j]], 0);
                                  //the slice from the shadow buffer
                                  gl.glMultiTexCoord2fv(GL.GL_TEXTURE2,lc[order[j]], 0);
                                  //the coords in screen space
                                  gl.glVertex2fv(lv[order[j]], 0);
                          }
                  }
                  gl.glEnd();

          }
          MakeCurrent();

          //now draw to the frame buffer
          gl.glBegin(GL.GL_POLYGON); //draw slice and texture map it
          {
                  for(int j=0; j< edges; ++j){
                          gl.glMultiTexCoord3fv(GL.GL_TEXTURE0,tc[order[j]], 0);
                          //glMultiTexCoord3fvARB(GL.GL_TEXTURE2,tc[order[j]]);
                          if((gluvv.shade == gluvvShade.gluvvShadeDiff) || (gluvv.shade == gluvvShade.gluvvShadeDSpec)){ //we have to use this for shadows
                                  gl.glMultiTexCoord3fv(GL.GL_TEXTURE3,lc[order[j]], 0);
                                  gl.glMultiTexCoord3fv(GL.GL_TEXTURE2,tc[order[j]], 0);
                          } else {
                                  gl.glMultiTexCoord3fv(GL.GL_TEXTURE2,lc[order[j]], 0);
                          }
                          gl.glVertex3fv(pc[order[j]], 0);
                  }
          }
          gl.glEnd();

          System.err.println("volren" + " end of shadow slice");

   }

   //============================================================ INTERSECT
//======================================================================
   /*
   const float p0[3], const float p1[3], //line end points
   const float t0[3], const float t1[3], //texture points
    const float v0[3], const float v1[3], //view coord points
    const float sp[3], const float sn[3], //plane point & norm
    float pnew[3], float tnew[3], float vnew[3]) //new values


    */
   private int intersect(
                        float[] p0, float[] p1, //line end points
                        float[] t0, float[] t1, //texture points
                        float[] v0, float[] v1, //view coord points
                        float[] sp, float[] sn, //plane point & norm
                        float[] pnew, float[] tnew, float[] vnew) //new values
   {
      //t = (sn.(sp - p0))/(sn.(p1 - p0))
      float t = ((sn[0]*(sp[0] - p0[0]) + sn[1]*(sp[1] - p0[1])
                  + sn[2]*(sp[2] - p0[2])) /
                 (sn[0]*(p1[0] - p0[0]) + sn[1]*(p1[1] - p0[1])
                  + sn[2]*(p1[2] - p0[2])));
      //note if the denominator is zero t is a NAN so we should have no problems?

      if( (t>=0) && (t<=1) ){
         //compute line intersection
         pnew[0] = p0[0] + t*(p1[0] - p0[0]);
         pnew[1] = p0[1] + t*(p1[1] - p0[1]);
         pnew[2] = p0[2] + t*(p1[2] - p0[2]);
         //compute texture interseciton
         tnew[0] = t0[0] + t*(t1[0] - t0[0]);
         tnew[1] = t0[1] + t*(t1[1] - t0[1]);
         tnew[2] = t0[2] + t*(t1[2] - t0[2]);
         //compute view coordinate intersections
         vnew[0] = v0[0] + t*(v1[0] - v0[0]);
         vnew[1] = v0[1] + t*(v1[1] - v0[1]);
         vnew[2] = v0[2] + t*(v1[2] - v0[2]);
         return 1;
      }
      return 0;
  }


  //========================================================= Render Slice
//======================================================================

  private void renderSlice(GLAutoDrawable drawable) {
    GL gl = drawable.getGL();
    gl.glPushMatrix();
    {
      gl.glMultMatrixf(gluvv.rinfo.xform, 0);
      gl.glDisable(GL.GL_LIGHTING);

      gl.glActiveTexture(GL.GL_TEXTURE0);
      {
        gl.glEnable(GL.GL_TEXTURE_3D);
        //glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
        //glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_TEXTURE_3D);
        gl.glBindTexture(GL.GL_TEXTURE_3D, texNames[0]);
      }

      gl.glColor4f(1, 1, 1, 1);
      gl.glBegin(GL.GL_QUADS);
      {
        gl.glMultiTexCoord3f(GL.GL_TEXTURE0, 0f, 0f, .5f);
        //glTexCoord3f(0.0,0.0,.5);
        gl.glVertex3f(0, 0, .5f);

        gl.glMultiTexCoord3f(GL.GL_TEXTURE0, 1f, 0f, .5f);
        //glTexCoord3f(1.0,0.0,.5);
        gl.glVertex3f(1f, 0f, .5f);

        gl.glMultiTexCoord3f(GL.GL_TEXTURE0, 1f, 1f, .5f);
        //glTexCoord3f(1.0,1.0,.5);
        gl.glVertex3f(1f, 1f, .5f);

        gl.glMultiTexCoord3f(GL.GL_TEXTURE0, 0f, 1f, .5f);
        //glTexCoord3f(0.0,1.0,.5);
        gl.glVertex3f(0f, 1f, .5f);
      }
      gl.glEnd();

      gl.glActiveTexture(GL.GL_TEXTURE0);
      {
        gl.glDisable(GL.GL_TEXTURE_3D);
        gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_NONE);
        gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_TEXTURE_3D);
      }

      gl.glEnable(GL.GL_TEXTURE_SHADER_NV);
      gl.glActiveTexture(GL.GL_TEXTURE0);
      {
        gl.glEnable(GL.GL_TEXTURE_3D);
        gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
        gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_TEXTURE_3D);
        gl.glBindTexture(GL.GL_TEXTURE_3D, texNames[0]);
      }
      gl.glActiveTexture(GL.GL_TEXTURE1);
      {
        gl.glEnable(GL.GL_TEXTURE_2D);
        gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
        gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV,
                  GL.GL_DEPENDENT_GB_TEXTURE_2D_NV);
        gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_PREVIOUS_TEXTURE_INPUT_NV,
                  GL.GL_TEXTURE0);
        gl.glBindTexture(GL.GL_TEXTURE_2D, deptexName[0]);
      }

      gl.glColor4f(1, 1, 1, 1);
      gl.glBegin(GL.GL_QUADS);
      {
        gl.glMultiTexCoord3f(GL.GL_TEXTURE0, 0f, 0f, .5f);
        gl.glVertex3f(0f, 0f, .5f);

        gl.glMultiTexCoord3f(GL.GL_TEXTURE0, 1f, 0f, .5f);
        gl.glVertex3f( -1f, 0f, .5f);

        gl.glMultiTexCoord3f(GL.GL_TEXTURE0, 1f, 1f, .5f);
        gl.glVertex3f( -1f, 1f, .5f);

        gl.glMultiTexCoord3f(GL.GL_TEXTURE0, 0f, 1f, .5f);
        gl.glVertex3f(0f, 1f, .5f);
      }
      gl.glEnd();

      gl.glActiveTexture(GL.GL_TEXTURE1);
      {
        gl.glDisable(GL.GL_TEXTURE_2D);
        gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_NONE);
        gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_NONE);
      }

      gl.glActiveTexture(GL.GL_TEXTURE0);
      {
        gl.glDisable(GL.GL_TEXTURE_3D);
        gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_NONE);
        gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_NONE);
      }

      gl.glDisable(GL.GL_TEXTURE_SHADER_NV);

      gl.glEnable(GL.GL_LIGHTING);

    }
    gl.glPopMatrix();

    System.err.println("NV20VolRen3D" + " Draw Slice");

  }


  //======================================================= Create Volumes
//======================================================================

  public void createVolumes(GLAutoDrawable drawable) {
    /*
                 if(!glTexImage3DEXT){
           go = 0;
           return;
                 }
     */
    if (gluvv.mv != null) {
      createBricks(drawable);
    }
    else {
      go = 0;
    }
  }

  //======================================================= Create Bricks
//======================================================================
  private void createBricks(GLAutoDrawable drawable) {
    GL gl = drawable.getGL();
    texNames = new int[gluvv.mv.numSubVols];
    shadeNames = new int[gluvv.mv.numSubVols];
    // gl.glGenTextures(gluvv.mv.numSubVols, texNames);          // ??????????????????????????????????????? array
    // gl.glGenTextures(gluvv.mv.numSubVols, shadeNames);        // ????????????????????????????????????   array

    int ne = gluvv.mv.nelts;

    for (int v = 0; v < gluvv.mv.numSubVols; ++v) {
      int sx = gluvv.mv.volumes[v].xiSize;
      int sy = gluvv.mv.volumes[v].yiSize;
      int sz = gluvv.mv.volumes[v].ziSize;

      byte[] d = gluvv.mv.volumes[v].currentData;
      byte[] tex3D;
      int i, j, k;

      if ( gluvv.dmode == gluvvDataMode.GDM_V1 || //one byte texture
           gluvv.dmode == gluvvDataMode.GDM_VGH_V ) {

          //this is where the texture data goes
          System.err.print("Creating 1 byte volume ");
          tex3D = new byte[sx * sy * sz];
          for (i = 0; i < sz; ++i) {
            for (j = 0; j < sy; ++j) {
              for (k = 0; k < sx; ++k) {
                tex3D[i * sx * sy + j * sx + k] = d[i * sx * sy * ne + j * sx * ne + k * ne + 0];   // ???   buffer copying
              }
            }
          }
          loadTex1B(drawable, tex3D, texNames[v], sx, sy, sz);
          System.err.println(" -done");
      } else if ( gluvv.dmode == gluvvDataMode.GDM_V1G ||//two byte texture
                  gluvv.dmode == gluvvDataMode.GDM_V2 ||
                  gluvv.dmode == gluvvDataMode.GDM_VGH_VG ) {
          System.err.print("Creating 2 byte volume ");
          tex3D = new byte[sx * sy * sz * 2];
          for (i = 0; i < sz; ++i) {
            for (j = 0; j < sy; ++j) {
              for (k = 0; k < sx; ++k) {
                tex3D[i * sx * sy * 2 + j * sx * 2 + k * 2 + 0] = d[i * sx * sy * ne + j * sx * ne + k * ne + 1];
                tex3D[i * sx * sy * 2 + j * sx * 2 + k * 2 + 1] = d[i * sx * sy * ne + j * sx * ne + k * ne + 0];
              }
            }
          }
          loadTex2B(drawable,tex3D, texNames[v], sx, sy, sz);
          tex3D = null;
          System.err.println(" -done");
      } else if ( gluvv.dmode == gluvvDataMode.GDM_V1GH || //3 or 4 byte data == 4 byte texture
                  gluvv.dmode == gluvvDataMode.GDM_V2G ||
                  gluvv.dmode == gluvvDataMode.GDM_V2GH ||
                  gluvv.dmode == gluvvDataMode.GDM_V3 ||
                  gluvv.dmode == gluvvDataMode.GDM_V3G ||
                  gluvv.dmode == gluvvDataMode.GDM_V4 ||
                  gluvv.dmode == gluvvDataMode.GDM_VGH ) {
          System.err.print("Creating 4 byte volume ");
          tex3D = new byte[sx * sy * sz * 4];
          for (i = 0; i < sz; ++i) {
            for (j = 0; j < sy; ++j) {
              for (k = 0; k < sx; ++k) {
                tex3D[i * sx * sy * 4 + j * sx * 4 + k * 4 +
                    0] = d[i * sx * sy * ne + j * sx * ne + k * ne + 1];
                tex3D[i * sx * sy * 4 + j * sx * 4 + k * 4 +
                    1] = d[i * sx * sy * ne + j * sx * ne + k * ne + 2];
                if (ne > 3)
                  tex3D[i * sx * sy * 4 + j * sx * 4 + k * 4 +
                      2] = d[i * sx * sy * ne + j * sx * ne + k * ne + 3];
                else
                  tex3D[i * sx * sy * 4 + j * sx * 4 + k * 4 + 2] = 0;
                tex3D[i * sx * sy * 4 + j * sx * 4 + k * 4 +
                    3] = d[i * sx * sy * ne + j * sx * ne + k * ne + 0];
              }
            }
          }
          loadTex4B(drawable, tex3D, texNames[v], sx, sy, sz);
          tex3D = null;
          System.err.println(" -done");
      } else {
          System.err.println("Error NV20VolRen3D2::createBricks(), data type unknown");
          return;
      }

      if (gluvv.mv.volumes[v].currentGrad != null) {
        System.err.print(" Down Loading Shade Texture " + v + " ... ");
        gl.glBindTexture(GL.GL_TEXTURE_3D, shadeNames[v]);
        gl.glPixelStorei(GL.GL_UNPACK_ALIGNMENT, 1);
        gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_WRAP_S, GL.GL_CLAMP);
        gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_WRAP_T, GL.GL_CLAMP);
        gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_WRAP_R, GL.GL_CLAMP);
        gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);
        gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR);

        ByteBuffer bBuff = ByteBuffer.wrap(gluvv.mv.volumes[v].currentGrad);
        bBuff.rewind();

        gl.glTexImage3D(GL.GL_TEXTURE_3D,
                        0,
                        GL.GL_RGB8,
                        sx,
                        sy,
                        sz,
                        0,
                        GL.GL_RGB,
                        GL.GL_UNSIGNED_BYTE,
                        bBuff);
      }

      gl.glFlush();
      System.err.println("NV20VolRen3D2" +  " Create Texture - Shade Texture");
      System.err.println("Done");

    } //for each sub-volume

    gl.glDisable(GL.GL_TEXTURE_3D);
  }

  //=================================================== load 1 byte volume
//======================================================================
  private void loadTex1B(GLAutoDrawable drawable, byte[] tex, int name, int sx, int sy, int sz) {
    GL gl = drawable.getGL();
    System.err.println(" Down Loading Data Texture " + name + " ... ");
    gl.glEnable(GL.GL_TEXTURE_3D);
    gl.glBindTexture(GL.GL_TEXTURE_3D, name);
    gl.glPixelStorei(GL.GL_UNPACK_ALIGNMENT, 1);
    gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_WRAP_S, GL.GL_CLAMP);
    gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_WRAP_T, GL.GL_CLAMP);
    gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_WRAP_R, GL.GL_CLAMP);
    gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);
    gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR);

    ByteBuffer bBuff = ByteBuffer.wrap(tex);
    bBuff.rewind();

    gl.glTexImage3D(GL.GL_TEXTURE_3D,
                    0,
                    GL.GL_ALPHA8,
                    sx,
                    sy,
                    sz,
                    0,
                    GL.GL_ALPHA,
                    GL.GL_UNSIGNED_BYTE,
                    bBuff);

    gl.glFlush();
    System.err.println("NV20VolRen3D2"+ " Create Texture - Data Texture");
    System.err.println("Done");
  }

  //=================================================== load 2 byte volume
//======================================================================
    private void loadTex2B(GLAutoDrawable drawable, byte[] tex, int name, int sx,
                           int sy, int sz) {
      GL gl = drawable.getGL();
      System.err.println(" Down Loading Data Texture " + name + " ... ");
      gl.glEnable(GL.GL_TEXTURE_3D);
      gl.glBindTexture(GL.GL_TEXTURE_3D, name);
      gl.glPixelStorei(GL.GL_UNPACK_ALIGNMENT, 1);
      gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_WRAP_S, GL.GL_CLAMP);
      gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_WRAP_T, GL.GL_CLAMP);
      gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_WRAP_R, GL.GL_CLAMP);
      gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);
      gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR);

      ByteBuffer bBuff = ByteBuffer.wrap(tex);
      bBuff.rewind();

      gl.glTexImage3D(GL.GL_TEXTURE_3D,
                      0,
                      GL.GL_LUMINANCE8_ALPHA8,
                      sx,
                      sy,
                      sz,
                      0,
                      GL.GL_LUMINANCE_ALPHA,
                      GL.GL_UNSIGNED_BYTE,
                      bBuff);

      gl.glFlush();
      System.err.println("NV20VolRen3D2" +  " Create Texture - Data Texture");
      System.err.println("Done");
  }


  //=================================================== load 4 byte volume
//======================================================================
    private void loadTex4B(GLAutoDrawable drawable, byte[] tex, int name, int sx, int sy, int sz) {
      GL gl = drawable.getGL();
      System.err.println(" Down Loading Data Texture " + name + " ... ");
      gl.glEnable(GL.GL_TEXTURE_3D);
      gl.glBindTexture(GL.GL_TEXTURE_3D, name);
      gl.glPixelStorei(GL.GL_UNPACK_ALIGNMENT, 1);
      gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_WRAP_S, GL.GL_CLAMP);
      gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_WRAP_T, GL.GL_CLAMP);
      gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_WRAP_R, GL.GL_CLAMP);
      gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);
      gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR);

      ByteBuffer bBuff = ByteBuffer.wrap(tex);
      bBuff.rewind();

      gl.glTexImage3D(GL.GL_TEXTURE_3D,
                      0,
                      GL.GL_RGBA8,
                      sx,
                      sy,
                      sz,
                      0,
                      GL.GL_RGBA,
                      GL.GL_UNSIGNED_BYTE,
                      bBuff);

      gl.glFlush();
      System.err.println("NV20VolRen3D2" +  " Create Texture - Data Texture");
      System.err.println("Done");
    }

    //==================================================== Create 2D Dep Tex
//======================================================================

    private void create2DDepTex(GLAutoDrawable drawable) {
      int i, j, k;
      GL gl = drawable.getGL();
      //------------- load gb deptex --------------------
      gl.glGenTextures(1, deptexName, 0); //the dep tex that we use for the tf
      gl.glGenTextures(1, gluvv.volren.deptexName, 0); //the dep tex that the tf widget sees

      int dsx = gluvv.tf.ptexsz[0];
      int dsy = gluvv.tf.ptexsz[1];

      deptex = new byte[dsx * dsy * 4]; //reference texture

      gluvv.volren.deptex = deptex;

      for (j = 0; j < dsy; ++j) {
        for (k = 0; k < dsx; ++k) {
          deptex[j * dsx * 4 + k * 4 + 0] = new Float(k / (float) dsx * 255).byteValue();
          deptex[j * dsx * 4 + k * 4 + 1] = new Float(j / (float) dsy * 255).byteValue(); //k==0 ? 0 : 255;
          deptex[j * dsx * 4 + k * 4 + 2] = new Float(255 - j / (float) dsy * 255).byteValue(); //k==0 ? 255 : 0;
          deptex[j * dsx * 4 + k * 4 +3] = new Float(j / (float) dsy * 255 / (float) 2).byteValue();
        }
      }

      gDeptex = new byte[dsx * dsy * 4]; //for good sample rates
      iDeptex = new byte[dsx * dsy * 4]; //for interactive sample rate
      copyScale(gluvv.volren.goodSamp, gDeptex);
      copyScale(gluvv.volren.interactSamp, iDeptex);

      lastSamp = gluvv.volren.goodSamp;
      lastGoodSamp = gluvv.volren.goodSamp;
      lastInteSamp = gluvv.volren.interactSamp;

      loadDepTex(drawable, gDeptex);

      gl.glEnable(GL.GL_TEXTURE_2D);
      gl.glBindTexture(GL.GL_TEXTURE_2D, gluvv.volren.deptexName[0]);

      gl.glPixelStorei(GL.GL_UNPACK_ALIGNMENT, 1);
      gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_S, GL.GL_CLAMP);
      gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_T, GL.GL_CLAMP);
      gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);
      gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR);

      ByteBuffer bBuff = ByteBuffer.wrap(deptex);
      bBuff.rewind();

      gl.glTexImage2D(GL.GL_TEXTURE_2D,
                   0,
                   GL.GL_RGBA8,
                   dsx,
                   dsy,
                   0,
                   GL.GL_RGBA,
                   GL.GL_UNSIGNED_BYTE,
                   bBuff);
      gl.glFlush();

      //--------------- load ar deptex ------------------
      gl.glGenTextures(1, deptex2Name, 0); //the dep tex that we use for the tf
      gl.glGenTextures(1, gluvv.volren.deptex2Name, 0); //the dep tex that the tf widget sees

      deptex2 = new byte[dsx * dsy * 4]; //reference texture

      gluvv.volren.deptex2 = deptex2;

      for (j = 0; j < dsy; ++j) {
        for (k = 0; k < dsx; ++k) {
          deptex2[j * dsx * 4 + k * 4 + 0] =new Float( k / (float) dsx * 255).byteValue();
          deptex2[j * dsx * 4 + k * 4 + 1] = new Float(j / (float) dsy * 255).byteValue(); //k==0 ? 0 : 255;
          deptex2[j * dsx * 4 + k * 4 + 2] = new Float(255 - j / (float) dsy * 255).byteValue(); //k==0 ? 255 : 0;
          deptex2[j * dsx * 4 + k * 4 + 3] = new Float(255).byteValue();
        }
      }

      loadDepTex(drawable, deptex2, deptex2Name[0]);

      gl.glEnable(GL.GL_TEXTURE_2D);
      gl.glBindTexture(GL.GL_TEXTURE_2D, gluvv.volren.deptex2Name[0]);

      gl.glPixelStorei(GL.GL_UNPACK_ALIGNMENT, 1);
      gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_S, GL.GL_CLAMP);
      gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_T, GL.GL_CLAMP);
      gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);
      gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR);

      ByteBuffer bBuffer = ByteBuffer.wrap(deptex2);
      bBuffer.rewind();

      gl.glTexImage2D(GL.GL_TEXTURE_2D,
                   0,
                   GL.GL_RGBA8,
                   dsx,
                   dsy,
                   0,
                   GL.GL_RGBA,
                   GL.GL_UNSIGNED_BYTE,
                   bBuffer);
      gl.glFlush();

      /*
              qnDeptex = new unsigned char[256*256*4];
              qnRef    = new float[256*256*3];

              for(int i=0; i<256; ++i){
                      for(int j=0; j<256; ++j){
                              qnRef[i*256*3 + j*3 + 0] = (i - 128)/(128.0);
                              qnRef[i*256*3 + j*3 + 1] = (j - 128)/(128.0);
                              qnRef[i*256*3 + j*3 + 2] = sqrt(1-(qnRef[i*256*3 + j*3 + 0]*qnRef[i*256*3 + j*3 + 0])-
       (qnRef[i*256*3 + j*3 + 1]*qnRef[i*256*3 + j*3 + 1]));
                      }
              }

              glGenTextures(1, &qnDeptexName);
       */

    }


    //========================================================= load Dep tex
//======================================================================
    private void loadDepTex(GLAutoDrawable drawable, byte[] dtex) {
      GL gl = drawable.getGL();
      gl.glEnable(GL.GL_TEXTURE_2D);
      gl.glBindTexture(GL.GL_TEXTURE_2D, deptexName[0]);

      gl.glPixelStorei(GL.GL_UNPACK_ALIGNMENT, 1);
      gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_S, GL.GL_CLAMP);
      gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_T, GL.GL_CLAMP);
      gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);
      gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR);

      ByteBuffer bBuff = ByteBuffer.wrap(dtex);
      bBuff.rewind();
      gl.glTexImage2D(GL.GL_TEXTURE_2D,
                   0,
                   GL.GL_RGBA8,
                   gluvv.tf.ptexsz[0],
                   gluvv.tf.ptexsz[1],
                   0,
                   GL.GL_RGBA,
                   GL.GL_UNSIGNED_BYTE,
                   bBuff);
      gl.glFlush();

    }

    private void loadDepTex(GLAutoDrawable drawable, byte[] dtex, int name) {
      GL gl = drawable.getGL();
      gl.glEnable(GL.GL_TEXTURE_2D);
      gl.glBindTexture(GL.GL_TEXTURE_2D, name);

      gl.glPixelStorei(GL.GL_UNPACK_ALIGNMENT, 1);
      gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_S, GL.GL_CLAMP);
      gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_T, GL.GL_CLAMP);
      gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);
      gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR);

      ByteBuffer bBuff = ByteBuffer.wrap(dtex);
      bBuff.rewind();

      gl.glTexImage2D(GL.GL_TEXTURE_2D,
                   0,
                   GL.GL_RGBA8,
                   gluvv.tf.ptexsz[0],
                   gluvv.tf.ptexsz[1],
                   0,
                   GL.GL_RGBA,
                   GL.GL_UNSIGNED_BYTE,
                   bBuff);
      gl.glFlush();
    }

    //==================================================== create Shadow tex
//======================================================================
    private void createShadowTex(GLAutoDrawable drawable)  //creates the shadow texture
    {
      GL gl = drawable.getGL();
      gl.glGenTextures(1, shadowName, 0);   //the dep tex that we use for the tf
      byte[] stex = new byte[gluvv.light.buffsz[0]*gluvv.light.buffsz[1]*4];

      gl.glEnable(GL.GL_TEXTURE_2D);
      gl.glBindTexture(GL.GL_TEXTURE_2D, shadowName[0]);
      gl.glPixelStorei(GL.GL_UNPACK_ALIGNMENT, 1);
      gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_S, GL.GL_CLAMP);
      gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_T, GL.GL_CLAMP);
      gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);
      gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR);

      ByteBuffer bBuff = ByteBuffer.wrap(stex);
      bBuff.rewind();

      gl.glTexImage2D(GL.GL_TEXTURE_2D,
                            0,
                            GL.GL_RGBA8,
                            gluvv.light.buffsz[0],
                            gluvv.light.buffsz[1],
                            0,
                            GL.GL_RGBA,
                            GL.GL_UNSIGNED_BYTE,
                            bBuff);
      gl.glFlush();
      gl.glDisable(GL.GL_TEXTURE_2D);
    }

    //=================================================== copy & scale alpha
//======================================================================

    private void scaleAlpha(float nsr, float osr, byte[] dtex)
    {
            if(nsr == osr) return;

            int dsx = gluvv.tf.ptexsz[0];
            int dsy = gluvv.tf.ptexsz[1];

            float alphaScale = 1/nsr;
            for(int i=0; i<dsy; ++i){
                    for(int j=0; j<dsx; ++j){
                            dtex[i*dsx*4 + j*4 + 3] =
                                    new Float((1f- Math.pow((1.0f- deptex[i*dsx*4 + j*4 + 3]/255.0f), alphaScale))*255f).byteValue();
                    }
            }
    }

    private void copyScale(float sr, byte[] dtex)
    {
            int dsx = gluvv.tf.ptexsz[0];
            int dsy = gluvv.tf.ptexsz[1];

            float alphaScale = 1.0f/sr;
            for(int i=0; i<dsy; ++i){
                    for(int j=0; j<dsx; ++j){
                            dtex[i*dsx*4 + j*4 + 0] = deptex[i*dsx*4 + j*4 + 0];
                            dtex[i*dsx*4 + j*4 + 1] = deptex[i*dsx*4 + j*4 + 1];
                            dtex[i*dsx*4 + j*4 + 2] = deptex[i*dsx*4 + j*4 + 2];
                            dtex[i*dsx*4 + j*4 + 3] =
                                    new Float((1.0f - Math.pow((1.0f-(deptex[i*dsx*4 + j*4 + 3]/255.0f)), alphaScale))*255).byteValue();
                    }
            }
    }



    //========================================================= make current
//======================================================================

   private void MakeCurrent()
   {
           // glutSetWindow(gluvv.mainWindow);
   }


}
