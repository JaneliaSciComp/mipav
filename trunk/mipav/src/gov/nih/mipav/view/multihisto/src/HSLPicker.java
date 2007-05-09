package gov.nih.mipav.view.multihisto.src;

public class HSLPicker {

  public static int HSLPWIDGNAME = 22243222;
  private float hue; //[0..1]
  private float lev; //[0..1]
  private float sat; //[0..1]

  public HSLPicker() {
    hue = 0f;
    sat = 1f;
    lev = 0.5f;
  }

  public void updateHL(float dh, float dl) {
    hue = (hue + dh) > 1.0 ? (hue + dh - 1.0f) :
        ( (hue + dh) < 0 ? (hue + dh + 1.0f) : (hue + dh));
    lev = (lev + dl) > 1 ? 1 : (lev + dl) < 0 ? 0 : (lev + dl);
  }

  public void updateS(float ds) {
    lev = (sat + ds) > 1 ? 1 : (sat + ds) < 0 ? 0 : (sat + ds);
  }

  public void getColor(float[] col) {
    float m1, m2, fract, mid1, mid2;
    int sextant;

    float H = hue;
    float S = sat;
    float L = lev;

    if (S == 0) {
      col[0] = col[1] = col[2] = L;
      return;
    }
    /* else there is hue */
    if (L <= 0.5)
      m2 = L * (1 + S);
    else
      m2 = L + S - L * S;
    m1 = 2 * L - m2;
    if (1.0 == H)
      H = 0;
    H *= 6;
    sextant = (int) Math.floor(H);
    fract = H - sextant;
    mid1 = m1 + fract * (m2 - m1);
    mid2 = m2 + fract * (m1 - m2);
    /* compared to HSVtoRGB: V -> m2, min -> m1 */
    switch (sextant) {
      case 0: {
        col[0] = m2;
        col[1] = mid1;
        col[2] = m1;
        break;
      }
      case 1: {
        col[0] = mid2;
        col[1] = m2;
        col[2] = m1;
        break;
      }
      case 2: {
        col[0] = m1;
        col[1] = m2;
        col[2] = mid1;
        break;
      }
      case 3: {
        col[0] = m1;
        col[1] = mid2;
        col[2] = m2;
        break;
      }
      case 4: {
        col[0] = mid1;
        col[1] = m1;
        col[2] = m2;
        break;
      }
      case 5: {
        col[0] = m2;
        col[1] = m1;
        col[2] = mid2;
        break;
      }
    }

  }

  public void reset(float h, float s, float l) {
    hue = h;
    lev = l;
    sat = s;

  }

}
