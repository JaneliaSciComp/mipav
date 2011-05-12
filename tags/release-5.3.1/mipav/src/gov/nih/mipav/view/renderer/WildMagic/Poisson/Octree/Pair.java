package gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree;

public class Pair<T,U>
{
    public T first;
    public U second;

    public Pair(T t, U u)
    {
        first = t;
        second = u;
    }
    
    public T getFirst()
    {
        return first; 
    }

    public U getSecond()
    {
        return second; 
    }

    
    public boolean equals(Object obj) {
      if (obj == null) return false;
      if ((obj.getClass() != this.getClass())) { //|| (obj.hashCode() != this.hashCode())) {
        return false;
      }
      
      return (this.getFirst().equals(((Pair) obj).getFirst()) && this.getSecond().equals(((Pair) obj).getSecond()));
    }
    
    /**
     * Define a hash code based on the first and second's hash code
     */
    public int hashCode() {
      return first.hashCode() ^ second.hashCode();
    }
    
    public String toString() {
      return "Pair(" + first + ", " + second + ")";
    }
  
} 