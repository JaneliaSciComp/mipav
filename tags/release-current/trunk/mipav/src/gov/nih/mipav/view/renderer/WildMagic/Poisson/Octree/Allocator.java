package gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree;

import java.util.*;

class AllocatorState{	
		public int index,remains;
}

public class Allocator<T> {
	
	int blockSize;
	int index,remains;
	
	public Vector<T[]> memory = new Vector<T[]>();
	
	public Allocator(){
		blockSize=index=remains=0;
	}
	
	public void dispose(){
		reset();
	}
	
	/** This method is the allocators destructor. It frees up any of the memory that
	  * it has allocated. */
	void reset(){
		for(int i=0;i<memory.size();i++){
			memory.set(i, null);
		}
		memory.clear();
		blockSize=index=remains=0;
	}

	/** This method returns the memory state of the allocator. */
	public AllocatorState getState() {
		AllocatorState s = new AllocatorState();
		s.index=index;
		s.remains=remains;
		return s;
	}
	
	/** This method rolls back the allocator so that it makes all of the memory previously
	  * allocated available for re-allocation. Note that it does it not call the constructor
	  * again, so after this method has been called, assumptions about the state of the values
	  * in memory are no longer valid. */
	public void rollBack(){
		if(memory.size() != 0 ){
			for(int i=0;i<memory.size();i++){
				  for(int j=0;j<blockSize;j++){
					memory.get(i)[j] = (T)new Object();
				  }
			}
			index=0;
			remains=blockSize;
		}
	}
	
	/** This method rolls back the allocator to the previous memory state and makes all of the memory previously
	  * allocated available for re-allocation. Note that it does it not call the constructor
	  * again, so after this method has been called, assumptions about the state of the values
	  * in memory are no longer valid. */
	public void rollBack(AllocatorState state){
		if(state.index<index || (state.index==index && state.remains<remains)){
			if(state.index<index){
				for(int j=state.remains;j<blockSize;j++){
					// memory[state.index].get(j).reset();
					memory.get(state.index)[j] = (T)new Object();
				}
				for(int i=state.index+1;i<index-1;i++){
					for(int j=0;j<blockSize;j++){
						// memory[i][j].~T();
						memory.get(i)[j] = (T)new Object();
					}
				}
				for(int j=0;j<remains;j++){
					// memory[index][j].~T();
					// new(&memory[index][j]) T();
					memory.get(index)[j] = (T)new Object();
				}
				index=state.index;
				remains=state.remains;
			}
			else{
				// for(int j=0;j<state.remains;j<remains){
				for(int j=0;j<state.remains && j< remains; j++){
					// memory[index][j].~T();
					// new(&memory[index][j]) T();
					memory.get(index)[j] = (T)new Object();
				}
				remains=state.remains;
			}
		}
	}

	/** This method initiallizes the constructor and the blockSize variable specifies the
	  * the number of objects that should be pre-allocated at a time. */
	public void set(int blockSize){
		reset();
		this.blockSize=blockSize;
		index=-1;
		remains=0;
	}
	
	/** This method returns a pointer to an array of elements objects. If there is left over pre-allocated
	  * memory, this method simply returns a pointer to the next free piece of memory, otherwise it pre-allocates
	  * more memory. Note that if the number of objects requested is larger than the value blockSize with which
	  * the allocator was initialized, the request for memory will fail.
	  */
	public T[] newElements(int elements){ //default elements=1
		T[] mem;
		if(elements == 0){return null;}
		if(elements>blockSize){
			System.err.println("Allocator Error, elements bigger than block-size: " + elements + " > " +  blockSize);
			return null;
		}
		if(remains<elements){
			if(index==memory.size()-1){
				mem=(T[])new Object[blockSize];
				if(mem == null ){ System.err.println("Failed to allocate memory\n"); System.exit(0);}
				memory.add(mem);
			}
			index++;
			remains=blockSize;
		}
		// mem= memory.get(index)[blockSize-remains];
		mem= memory.get(index);
		remains-=elements;
		return mem;
	}
	
	
}