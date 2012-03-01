
$(document).ready(function(){
       var n = 0;
       var next=n;
       
	$(window).keydown(function(e){
		switch(e.keyCode){
		case 37: next = 0; break;
		case 38: if(n > 0)next = n-1; break;
		case 39: next = size -1; break;
		case 40: if(n < size-1) next = n+1; break;
		default: break;
		};
		if(n != next){
		    slides.eq(n).hide();
		    slides.eq(next).show();
		};
		n = next;
	    });
	
	var slides=$('.slide');
	var size = slides.size();
	$('.slide').hide();

	// show the first slide
	slides.eq(n).show();
   
 });
