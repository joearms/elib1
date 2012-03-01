function setup()
{
    $(".xright").each(function(i) {
            var text = $(this).attr('alt');
	    // alert(text);
	    $(this).wrap("<div class='right'></div>");
	    $(this).parent().append("<div class='caption'>" +
				    text + "</div>"); 
	});
    $(".xleft").each(function(i) {
            var text = $(this).attr('alt');
	    // alert(text);
	    $(this).wrap("<div class='left'></div>");
	    $(this).parent().append("<div class='caption'>" +
	                            text + "</div>"); 
	});
}

$(document).ready(function(){setup()});
