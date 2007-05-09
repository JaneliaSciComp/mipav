package gov.nih.mipav.view.multihisto.src;

public class TFPoint {

  public float[] v = new float[3];   //the user defined vector
  public float[] pos = new float[3]; //the values [0-1]
  public HSLPicker   cpick; //color picker

  private TFPoint next, prev;
  private int bin;
  private int type;

  public TFPoint()
  { //we are a dummy point;
     bin = -1;
     pos[0] = 0;
     pos[1] = 0;
     pos[2] = 0;
  }

  public TFPoint(int b, float x, float y, float z)
  {
     cpick = new HSLPicker();
     pos[0]=x;
     pos[1]=y;
     pos[2]=z;
     next = prev = null;
     bin = b;
  }

  public float[] getPos(){
    return pos;
  }

  public void setPos(float[] p) {
    pos[0]=p[0];
    pos[1]=p[1];
    pos[2]=p[2];
  }

  public void setPosU(float u) {
    pos[0] = u;
  }

  public void setPosV(float v) {
    pos[1] = v;
  }

  public void setPosW(float w) {
    pos[2] = w;
  }

  public void setV(float a, float b, float c) {
    v[0]=a;
    v[1]=b;
    v[2]=c;
  }

  public int getType() {
    return type;
  }

  public int getBin() {
    return bin;
  }

  public void setBin(int b) {
    bin = b;
  }

  /****** ??????????????????  C++ operator ****************/
  public float operator(int i) {
    return pos[i];
  }

  public TFPoint insert(TFPoint p)
  {
          if(bin < p.getBin()){
                  if(next != null){
                          next.insert(p);
                  }
                  else{ //no next
                          next = p;
                          p.prev = this;
                          p.next = null;
                  }
                  return this;
          }
          else{
                  p.prev = prev;
                  p.next = this;
                  if(prev != null)
                          prev.next = p;
                  prev = p;
                  return p;
          }
  }

  public TFPoint getPoint(int b)
  {
          if(bin == b){
                  if(prev != null){
                          prev.next = next;
                  }
                  if(next != null){
                          next.prev = prev;
                  }
                  this.prev = null;
                  this.next = null;
                  return this;
          }
          else{
                  if(next != null)
                          return next.getPoint(b);
                  else
                          return null;
          }
  }


  public  TFPoint findPoint(int b)
  {
          if(bin == b){
                  return this;
          }
          else{
                  if(next != null)
                          return next.findPoint(b);
                  else
                          return null;
          }
  }



}
