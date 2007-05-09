package gov.nih.mipav.view.multihisto.src;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import com.sun.opengl.util.*;
import java.nio.*;

public class TLUT {

  protected float[] theTable;
  protected float[] _rgba;
  protected int _size;
  protected float _alpha;
  protected float lastSampleRate;
  protected float lastAlphaScale;

  public static int TLUTEnums = 4;
  public static int _numElts = 4;

  public TLUT() {

  }

  public TLUT(int size) {
    _size = size;
    _rgba = new float[_numElts * size];
    theTable = new float[_numElts * size];
    _alpha = (float) (1.0f / _size);
    rgbGrayScaleRamp();
    alphaConstant(_alpha);
    lastSampleRate = 1.0f;
    lastAlphaScale = 1.0f;
  }

  //***************************************************************
   // TLUT LOADING METHODS
   //***************************************************************
    public void loadTransferTableRGBA(GLAutoDrawable drawable) {

      GL gl = drawable.getGL();
      // this is an optional scaling factor. if you want identity scaling
      // (scale = 1), then you could skip theTable construction and
      // pass this->GetRGBA(0) instead of theTable to glColorTableSGI()
      float scale = 1;

      float[] rgba = new float[4];
      /*
           #ifdef WIN32
           #if    WILDCAT
        glEnable(GL_TEXTURE_COLOR_TABLE_EXT);
           #endif
           #else
        glEnable(GL_TEXTURE_COLOR_TABLE_SGI);
           #endif
           gl.glEnable(GL.GL_TEXTURE_COLOR_TABLE_EXT);
           gl.glEnable(GL.GL_TEXTURE_COLOR_TABLE_SGI);
       */
      for (int n = 0; n < _size; ++n) {
        // rgba = this.GetRGBA(n);             // ?????????????????????  get RGBA
        rgba[0] = _rgba[n * _numElts + 0];
        rgba[1] = _rgba[n * _numElts + 1];
        rgba[2] = _rgba[n * _numElts + 2];
        rgba[3] = _rgba[n * _numElts + 3];
        theTable[n * _numElts + 0] = rgba[0] * scale * rgba[3];
        theTable[n * _numElts + 1] = rgba[1] * scale * rgba[3];
        theTable[n * _numElts + 2] = rgba[2] * scale * rgba[3];
        theTable[n * _numElts + 3] = rgba[3] * scale;
      }
      FloatBuffer theTableBuf = FloatBuffer.wrap(theTable);
      theTableBuf.rewind();
      /*
           #ifdef WIN32
           #if    WILDCAT
        glColorTableEXT(GL_TEXTURE_COLOR_TABLE_EXT, GL_RGBA8, _size,
                        GL_RGBA, GL_FLOAT, theTable);
           #endif
           #else
        glColorTableSGI(GL_TEXTURE_COLOR_TABLE_SGI, GL_RGBA8_EXT, _size,
                        GL_RGBA, GL_FLOAT, theTable);
          #endif
           gl.glColorTableEXT(GL.GL_TEXTURE_COLOR_TABLE_SGI, GL.GL_RGBA8, _size, GL.GL_RGBA, GL.GL_FLOAT, theTableBuf);
           gl.glColorTableEXT(GL.GL_TEXTURE_COLOR_TABLE_EXT, GL.GL_RGBA8, _size, GL.GL_RGBA, GL.GL_FLOAT, theTableBuf);
       */
    }

  public void loadTransferTableALPHA() {
    float[] rgba = new float[4];
    float alpha;                         // ????????????????????//  RGBA
    float[] alphaTable = new float[_size];
    for (int n = 0; n < _size; ++n) {
      // rgba = this.GetRGBA(n);
      rgba[3] = _rgba[n * _numElts + 3];
      alpha = rgba[3];
      alphaTable[n] = alpha;
    }
    /*
       #ifdef WIN32
       #if    WILDCAT
      glColorTableEXT(GL_TEXTURE_COLOR_TABLE_EXT, GL_ALPHA, _size,
                      GL_RGBA, GL_FLOAT, alphaTable);
       #endif
       #else
      glColorTableSGI(GL_TEXTURE_COLOR_TABLE_SGI, GL_ALPHA, _size,
                      GL_ALPHA, GL_FLOAT, alphaTable);
       #endif
       gl.glColorTableEXT(GL.GL_TEXTURE_COLOR_TABLE_EXT, GL.GL_ALPHA, _size,
                      GL.GL_RGBA, GL.GL_FLOAT, alphaTable);
     */

  }

  public void copyTable(float[] cmap, float scale) {
    for (int n = 0; n < _size; ++n) {
      _rgba[n * _numElts + 0] = cmap[n * _numElts + 0];
      _rgba[n * _numElts + 1] = cmap[n * _numElts + 1];
      _rgba[n * _numElts + 2] = cmap[n * _numElts + 2];
      _rgba[n * _numElts + 3] = cmap[n * _numElts + 3] * scale;
    }
  }


  //***************************************************************
  // CHANNEL MODIFIERS
  //***************************************************************
  public void channelConstant(int channelIndex, float a) {
      for (int n = 0; n < _size; ++n) {
        _rgba[n * _numElts + channelIndex] = a;
      }
    }

  public void channelRamp(int channelIndex, int startIndex, int stopIndex, float startValue, float stopValue)
  {
    float denom = stopIndex - startIndex;
    float range = stopValue - startValue;

    for(int n = startIndex; n <= stopIndex; ++n){
      float alpha = startValue + range*(n - startIndex)/denom;
      _rgba[n*_numElts+channelIndex] = alpha;
    }
  }

  public void scaleAlpha(GLAutoDrawable drawable, float sampleRate)
  {
     //for an explantation of this see 'gkalpha.txt'
     if(lastSampleRate == sampleRate) return;

     float alphaScale = lastSampleRate/sampleRate;
     //float *rgba;
     lastSampleRate = sampleRate;
     //it is a damn good thing we dont have many entries in the TLUT
     for(int i=0; i<_size; ++i){

        _rgba[i *_numElts + 3] = 1.0f- (float)Math.pow((double)(1.d- _rgba[i*_numElts +3]), (double)alphaScale);
     }

     loadTransferTableRGBA(drawable);

  }


  public void linearScaleAlpha(GLAutoDrawable drawable,float scale)
  {
     if(lastAlphaScale == scale) return;

     float alphaScale = scale/lastAlphaScale;
     lastAlphaScale = scale;
     System.out.println("scaling alpha by:" + alphaScale);;
     for(int i=0; i< _size; ++i){
        _rgba[i *_numElts + 3] = _rgba[i*_numElts +3] * alphaScale;
     }
     loadTransferTableRGBA(drawable);
  }

  //***************************************************************
  // RGB MODIFIERS (PREDEFINED COLORMAPS)
  //***************************************************************
  public void rgbConstant(float r, float g, float b) {
      for (int n = 0; n < _size; ++n) {
        _rgba[n * _numElts] = r;
        _rgba[n * _numElts + 1] = g;
        _rgba[n * _numElts + 2] = b;
      }
    }

  public void rgbGrayScaleRamp() {
    for (int n = 0; n < _size; ++n) {
      _rgba[n * _numElts] = _rgba[n * _numElts + 1] = _rgba[n * _numElts + 2] = n / (float) (_size - 1.0);
    }
  }

  public void rgbRedScaleRamp() {
    for (int n = 0; n < _size; ++n) {
      _rgba[n * _numElts] = 0.9f;
      _rgba[n * _numElts + 1] = _rgba[n * _numElts + 2] = 0.9f * n / (float) (_size - 1.0f);
    }
  }

  public void rgbSpectral()
  {
    int n;
    float t;

    float r1 = 238f/255.0f;
    float g1 = 138f/255.0f;
    float b1 = 238f/255.0f;
    float r2 = 25f/255.0f;
    float g2 = 25f/255.0f;
    float b2 = 112f/255.0f;
    int min = 0;
    int max = _size/16;
    for(n = min; n < max; ++n){
      t = (n-min)/(max-min-1.0f);
      _rgba[n*_numElts+0] = (1f-t) * r1 + t * r2;
      _rgba[n*_numElts+1] = (1f-t) * g1 + t * g2;
      _rgba[n*_numElts+2] = (1f-t) * b1 + t * b2;
    }

    r1 = r2;  g1 = g2;  b1 = b2;
    r2 = 0f/255.0f;
    g2 = 0f/255.0f;
    b2 = 255f/255.0f;
    min = max;
    max += 3*_size/16;
    for(n = min; n < max; ++n){
      t = (n-min)/(max-min-1.0f);
      _rgba[n*_numElts+0] = (1f-t) * r1 + t * r2;
      _rgba[n*_numElts+1] = (1f-t) * g1 + t * g2;
      _rgba[n*_numElts+2] = (1f-t) * b1 + t * b2;
    }

    r1 = r2;  g1 = g2;  b1 = b2;
    r2 = 0f/255.0f;
    g2 = 255f/255.0f;
    b2 = 0f/255.0f;
    min = max;
    max += 3*_size/16;
    for(n = min; n < max; ++n){
      t = (n-min)/(max-min-1.0f);
      _rgba[n*_numElts+0] = (1f-t) * r1 + t * r2;
      _rgba[n*_numElts+1] = (1f-t) * g1 + t * g2;
      _rgba[n*_numElts+2] = (1f-t) * b1 + t * b2;
    }

    r1 = r2;  g1 = g2;  b1 = b2;
    r2 = 173f/255.0f;
    g2 = 252f/255.0f;
    b2 = 0f/255.0f;
    min = max;
    max += 3*_size/16;
    for(n = min; n < max; ++n){
      t = (n-min)/(max-min-1.0f);
      _rgba[n*_numElts+0] = (1f-t) * r1 + t * r2;
      _rgba[n*_numElts+1] = (1f-t) * g1 + t * g2;
      _rgba[n*_numElts+2] = (1f-t) * b1 + t * b2;
    }

    r1 = r2;  g1 = g2;  b1 = b2;
    r2 = 255f/255.0f;
    g2 = 255f/255.0f;
    b2 = 0f/255.0f;
    min = max;
    max += 1*_size/16;
    for(n = min; n < max; ++n){
      t = (n-min)/(max-min-1.0f);
      _rgba[n*_numElts+0] = (1f-t) * r1 + t * r2;
      _rgba[n*_numElts+1] = (1f-t) * g1 + t * g2;
      _rgba[n*_numElts+2] = (1f-t) * b1 + t * b2;
    }

    r1 = r2;  g1 = g2;  b1 = b2;
    r2 = 255f/255.0f;
    g2 = 165f/255.0f;
    b2 = 0f/255.0f;
    min = max;
    max += 2*_size/16;
    for(n = min; n < max; ++n){
      t = (n-min)/(max-min-1.0f);
      _rgba[n*_numElts+0] = (1f-t) * r1 + t * r2;
      _rgba[n*_numElts+1] = (1f-t) * g1 + t * g2;
      _rgba[n*_numElts+2] = (1f-t) * b1 + t * b2;
    }

    r1 = r2;  g1 = g2;  b1 = b2;
    r2 = 255f/255.0f;
    g2 = 0f/255.0f;
    b2 = 0f/255.0f;
    min = max;
    max += 3*_size/16;
    for(n = min; n < max; ++n){
      t = (n-min)/(max-min-1.0f);
      _rgba[n*_numElts+0] = (1f-t) * r1 + t * r2;
      _rgba[n*_numElts+1] = (1f-t) * g1 + t * g2;
      _rgba[n*_numElts+2] = (1f-t) * b1 + t * b2;
    }
  }

  public void rgbCyanMagenta()
  {
    float r1 = 1.0f;
    float g1 = 0.0f;
    float b1 = 1.0f;
    float r2 = 0.0f;
    float g2 = 1.0f;
    float b2 = 1.0f;
    float t;
    for(int n = 0; n < _size; ++n){
      t = n / (float) (_size - 1.0f);
      _rgba[n*_numElts+0] = (1f-t) * r1 + t * r2;
      _rgba[n*_numElts+1] = (1f-t) * g1 + t * g2;
      _rgba[n*_numElts+2] = (1f-t) * b1 + t * b2;
    }
  }

  public void rgbRobertsons()
  {
    int n;
    float t;

    float r1 = 0f/255.0f;
    float g1 = 0f/255.0f;
    float b1 = 255f/255.0f;
    float r2 = 25f/255.0f;
    float g2 = 25f/255.0f;
    float b2 = 230f/255.0f;
    int min = 0;
    int max = _size/6;
    for(n = min; n < max; ++n){
      t = (float)(n-min)/(float)(max-min-1.0f);
      _rgba[n*_numElts+0] = (1f-t) * r1 + t * r2;
      _rgba[n*_numElts+1] = (1f-t) * g1 + t * g2;
      _rgba[n*_numElts+2] = (1f-t) * b1 + t * b2;
    }

    r1 = r2;  g1 = g2;  b1 = b2;
    r2 = 55f/255.0f;
    g2 = 55f/255.0f;
    b2 = 200f/255.0f;
    min = max;
    max += _size/6;
    for(n = min; n < max; ++n){
      t = (float)(n-min)/(float)(max-min-1.0f);
      _rgba[n*_numElts+0] = (1f-t) * r1 + t * r2;
      _rgba[n*_numElts+1] = (1f-t) * g1 + t * g2;
      _rgba[n*_numElts+2] = (1f-t) * b1 + t * b2;
    }

    r1 = r2;  g1 = g2;  b1 = b2;
    r2 = 127f/255.0f;
    g2 = 127f/255.0f;
    b2 = 127f/255.0f;
    min = max;
    max += _size/6;
    for(n = min; n < max; ++n){
      t = (float)(n-min)/(float)(max-min-1.0f);
      _rgba[n*_numElts+0] = (1.0f-t) * r1 + t * r2;
      _rgba[n*_numElts+1] = (1f-t) * g1 + t * g2;
      _rgba[n*_numElts+2] = (1f-t) * b1 + t * b2;
    }

    r1 = r2;  g1 = g2;  b1 = b2;
    r2 = 200f/255.0f;
    g2 = 200f/255.0f;
    b2 = 55f/255.0f;
    min = max;
    max += _size/6;
    for(n = min; n < max; ++n){
      t = (float)(n-min)/(float)(max-min-1.0f);
      _rgba[n*_numElts+0] = (1f-t) * r1 + t * r2;
      _rgba[n*_numElts+1] = (1f-t) * g1 + t * g2;
      _rgba[n*_numElts+2] = (1f-t) * b1 + t * b2;
    }

    r1 = r2;  g1 = g2;  b1 = b2;
    r2 = 230f/255.0f;
    g2 = 230f/255.0f;
    b2 = 25f/255.0f;
    min = max;
    max += _size/6;
    for(n = min; n < max; ++n){
      t = (float)(n-min)/(float)(max-min-1.0);
      _rgba[n*_numElts+0] = (1f-t) * r1 + t * r2;
      _rgba[n*_numElts+1] = (1f-t) * g1 + t * g2;
      _rgba[n*_numElts+2] = (1f-t) * b1 + t * b2;
    }

    r1 = r2;  g1 = g2;  b1 = b2;
    r2 = 255f/255.0f;
    g2 = 255f/255.0f;
    b2 = 0f/255.0f;
    min = max;
    max += _size/6;
    for(n = min; n < max; ++n){
      t = (float)(n-min)/(float)(max-min-1.0f);
      _rgba[n*_numElts+0] = (1f-t) * r1 + t * r2;
      _rgba[n*_numElts+1] = (1f-t) * g1 + t * g2;
      _rgba[n*_numElts+2] = (1f-t) * b1 + t * b2;
    }

    // pad extra cells with final value
    for(n = max; n < _size; ++n){
      _rgba[n*_numElts+0] = r2;
      _rgba[n*_numElts+1] = g2;
      _rgba[n*_numElts+2] = b2;
    }
  }


  public void rgbMountain()
  {
    float r1 = 0.0f;
    float g1 = 0.0f;
    float b1 = 0.0f;
    float r2 = 0.0f;
    float g2 = 0.0f;
    float b2 = 1.0f;
    float t;
    int n;
    for(n = 0; n < _size/4; ++n){
      t = n / (float) (_size/4 - 1.0f);
      _rgba[n*_numElts+0] = (1f-t) * r1 + t * r2;
      _rgba[n*_numElts+1] = (1f-t) * g1 + t * g2;
      _rgba[n*_numElts+2] = (1f-t) * b1 + t * b2;
    }
    r1 = r2;  g1 = g2;  b1 = b2;
    r2 = 1.0f;
    g2 = 1.0f;
    b2 = 1.0f;

    float min = _size/4;
    float max = _size;
    for(n = (int)min; n < (int)max; ++n){
      t = (float)(n-min)/(float)(max-min-1.0f);
      _rgba[n*_numElts+0] = (1f-t) * r1 + t * r2;
      _rgba[n*_numElts+1] = (1f-t) * g1 + t * g2;
      _rgba[n*_numElts+2] = (1f-t) * b1 + t * b2;
    }

    // pad extra cells with final value
    for(n = (int)max; n < _size; ++n){
      _rgba[n*_numElts+0] = r2;
      _rgba[n*_numElts+1] = g2;
      _rgba[n*_numElts+2] = b2;
    }
  }


  public double AFFINE(double i, double x, double I, double o, double O) {
    return ((double)(O)-(o))*((double)(x)-(i)) / ((double)(I)-(i)) + (o);
  }

  public void rgbBlackBody()
  {
     int i;

     for(i=0; i<_size/3; ++i){//ramp up red, down blue
        _rgba[i*_numElts + 0] = (float)AFFINE(0d, i, _size/3d, 0d, 1d);
        _rgba[i*_numElts + 1] = 0f;
        _rgba[i*_numElts + 2] = (float)AFFINE(0d, i, _size/3d, .4d, 0d);
     }
     for(i=_size/3; i<2*_size/3; ++i){//ramp up green (yellow)
        _rgba[i*_numElts + 0] = 1f;
        _rgba[i*_numElts + 1] = (float)AFFINE(_size/3d, i, 2d*_size/3d, 0d, 1d);
        _rgba[i*_numElts + 2] = 0f;
     }
     for(i=2*_size/3; i<_size; ++i){//ramp up blue (white)
        _rgba[i*_numElts + 0] = 1f;
        _rgba[i*_numElts + 1] = 1f;
        _rgba[i*_numElts + 2] = (float)AFFINE(2d*_size/3d, i, _size, 0d, 1.05d);
     }

   }


   // modify individual channels
   public void redConstant(float alpha) {
     channelConstant(0, alpha);
   }

   public void redRamp(int startIndex, int stopIndex, float startValue, float stopValue) {
     channelRamp(0, startIndex, stopIndex, startValue, stopValue);
   }

   public void greenConstant(float alpha) {
     channelConstant(1, alpha);
   }

   public void greenRamp(int startIndex, int stopIndex, float startValue, float stopValue) {
     channelRamp(1, startIndex, stopIndex, startValue, stopValue);
   }

   public void blueConstant(float alpha) {
     channelConstant(2, alpha);
   }

   public void blueRamp(int startIndex, int stopIndex, float startValue, float stopValue) {
     channelRamp(2, startIndex, stopIndex, startValue, stopValue);
   }

   public void alphaConstant(float alpha) {
     channelConstant(3, alpha);
   }

   public void alphaRamp(int startIndex, int stopIndex, float startValue, float stopValue) {
     channelRamp(3, startIndex, stopIndex, startValue, stopValue);
   }


  //***************************************************************
// ELEMENT ACCESSORS AND  MODIFIERS
//***************************************************************
    public int GetSize() {
      return _size;
    }

  public float[] GetRGBA(float t) {
    // int offset = (int) (t * (_size - 1));
    // return _rgba[offset * _numElts];
    return _rgba;
  }
  /*
  public void GetRGBA(float t, float[] r, float[] g, float[] b, float[] a) {
    int offset = (int) (t * (_size - 1) * _numElts);
    r[0] = _rgba[offset];
    g[0] = _rgba[offset + 1];
    b[0] = _rgba[offset + 2];
    a[0] = _rgba[offset + 3];
  }

  public float[] GetRGBA(int n) {
    return _rgba[n * _numElts];
  }
  */
  public void SetRGBA(int n, float r, float g, float b, float a) {
    SetRGB(n, r, g, b);
    SetAlpha(n, a);
  }

  public void SetRGB(int n, float r, float g, float b) {
    int offset = n * _numElts;
    _rgba[offset] = r;
    _rgba[offset + 1] = g;
    _rgba[offset + 2] = b;
  }

  public void SetAlpha(int n, float a) {
    int offset = n * _numElts;
    _rgba[offset + 3] = a;
  }

  public void SetBitwiseRGBA(int n, float r, float g, float b, float a) {
    SetBitwiseRGB(n, r, g, b);
    SetBitwiseAlpha(n, a);
  }

  public void SetBitwiseRGB(int n, float r, float g, float b) {
    // unsigned ??????????????????????????/
    // System.err.println("SetBitwiseRGB");
    int mask = 0x1 << n;
    for (int k = 0; k < (int) _size; ++k) {
      if ( (k & mask) == 1) {
        int offset = k * _numElts;
        _rgba[offset] = r;
        _rgba[offset + 1] = g;
        _rgba[offset + 2] = b;
      }
    }
  }

  public void SetBitwiseAlpha(int n, float a) {
    // unsigned int _size ??????????????????????????
    // System.err.println("SetBitwiseAlpha");
    int mask = 0x1 << n;
    for (int k = 0; k < _size; ++k) {
      if ( (k & mask) == 1) {
        int offset = k * _numElts;
        _rgba[offset + 3] = a;
      }
    }
  }

}
