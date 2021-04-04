jQuery(document).ready(function($){

	

	

	$(document).on('click','.mittun-classy-donate',function(){

	var elem=$(this);

	var src=elem.attr('data-mfp-src');

	var skin=elem.closest('.mittun-classy').hasClass('skin_2')?'skin_2':'skin_1';
	
	if($(src).length){

	

		$('#eid').val(elem.attr('data-campaign-id'));	
		
		
			$.magnificPopup.open({

			  items: {

				src: src, // can be a HTML string, jQuery object, or CSS selector

				type: 'inline'

			  },

			  callbacks: {

				open: function() {
					
					this.content.addClass(skin);
					
					 $(src).find("input:button").removeClass('active');					 

				},
				

				},

			});
		

		

		return false;

	}

	

	});
	
	
	
	
	$(document).on('keyup','input[name="amount"]',function(){  
		var valid = /^\d+(\.\d{0,2})?$/.test(this.value),
        val = $(this).val();
    
		if(!valid){
			console.log("Invalid input!");
			$(this).val(val.substring(0, val.length - 1));
		}
	});

	$(document).on('click','.classy-amount input:button',function(){

		

		var parent=$(this).closest('p');

		var parentForm=parent.closest('form');

		

		parent.find("input:button").removeClass('active');

		$(this).addClass('active');

		

		var amt=$(this).attr('data-amount');

		

		if(typeof amt==='undefined'){

			parentForm.find("input[name='amount']").val('').focus();

		}

		else

		{

			parentForm.find("input[name='amount']").val(amt);

		}

		

		

	});

					

	$(document).on('click','.classy-donation-form.short input[name="recurring"]',function(){
		$(this).closest('form').submit();																					   
	});
	
	
});
function mittun_classy_notification()
{
	var data = {
		'action': 'mittun_classy_notification'
	};
	jQuery.post(mittunClassy.ajax_url, data, function(response) {
		jQuery('#mittun-classy-notification').html(response);
		setTimeout(function(){ mittun_classy_notification(); }, 30000);
	});	
}