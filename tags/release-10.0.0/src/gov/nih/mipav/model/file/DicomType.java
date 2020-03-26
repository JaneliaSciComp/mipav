package gov.nih.mipav.model.file;

public interface DicomType {

    public Object[] read(byte[] data);
    
    public byte[] write(Object obj);
}
