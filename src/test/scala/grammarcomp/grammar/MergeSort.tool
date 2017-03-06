program MergeSort {
	do(new MS().Start(10));
}

class MS { 
	/*here, we put a commentary:
	for testing purposes*/
	var number : Int[];
	var size : Int;
	var temp : Int[];
	//Here is another one.
	def Start(sz: Int):Int= {
		do(this.Init(sz));
		do(this.Print());
		println("___");
		do(this.Sort(0, size-1));
		do(this.Print());
		return 9999;
	}
	
	def Sort(left : Int, right : Int) : Int = {
		var mid : Int;
		if (left<right){
			mid = ((left + right)/2);
			
			do(this.Sort(left, mid));
			do(this.Sort(mid+1, right));
			do(this.Merge(left, right, mid));
		}
		
		return 0;
	}
	
	def Merge(left : Int, right : Int, mid: Int) : Int = {
		var i : Int;
		var j : Int;
		var tmpL : Int;
		var tmpR: Int;
		var where : Int;
		
		i=left;
		j=mid+1;
		where=i;
		while((i<(mid+1))&&(j<(right+1))) {
			tmpL=number[i];
			tmpR=number[j];
			if (tmpL<tmpR) {
				temp[where]=tmpL;
				i=i+1;
				where=where+1;
				
			}
			else {
				temp[where]=tmpR;
				j=j+1;
				where=where+1;
			}
		}
		while (i<(mid+1)) {
			temp[where]=number[i];
			i=i+1;
			where=where+1;
		}
		while (j<(right+1)) {
			temp[where]=number[j];
			j=j+1;
			where=where+1;
		}
		do(this.CopyTemp(left, right));
		return 0;
	}
	
	def CopyTemp(left: Int, right: Int): Int = {
		var i: Int;
		
		i=left;
		while(i<(right+1)) {
			number[i]=temp[i];
			i=i+1;
		}
		return 0;
	}
	
	def Print() : Int = {
        var j : Int;

        j = 0 ;
        while (j < (size)) {
            println(number[j]);
            j = j + 1 ;
        }
        return 0 ;
    }
	
	def Init(sz : Int) : Int = {
        size = sz ;
        number = new Int[sz] ;

        number[0] = 20 ;
        number[1] = 7  ; 
        number[2] = 12 ;
        number[3] = 18 ;
        number[4] = 2  ; 
        number[5] = 11 ;
        number[6] = 6  ; 
        number[7] = 9  ; 
        number[8] = 19 ; 
        number[9] = 5  ;

		temp = new Int[sz];

        return 0 ;  
    }
}