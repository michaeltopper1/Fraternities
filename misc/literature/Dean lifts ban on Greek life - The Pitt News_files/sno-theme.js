// make full tag clickable
jQuery(document).ready(function() {
	jQuery('li.blockstag').click(function() {
		if (jQuery(this).find('a').length) {
			window.location=jQuery(this).find('a').attr('href');
		}
	});
});

// story list widget
jQuery(document).ready(function() {
	jQuery('.sno-story-card, .dual-format-card').click(function() {
		if (jQuery(this).find('a.homeheadline').length) {
			window.location=jQuery(this).find('a.homeheadline').attr('href');
		}
	});
});


// make sure that all story list cards in a horizontal row have the same height to create a nice, even display

jQuery(window).load(function() {
	
	jQuery('.list-horizontal').each(function() {
		var height = 0; var tallest = 0; 
		jQuery(this).find('.sno-story-card').each(function() {
			height = jQuery(this).height();
			tallest = (height > tallest) ? height : tallest;
		}); 
		jQuery(this).find('.sno-story-card').height(tallest);
	});

});

// parallax widget
jQuery(document).ready(function() {
	
	jQuery('.bcg').click(function() {
		window.location=jQuery(this).find('a').attr('href');
	});
	
	jQuery('.text-parallax').click(function() {
		window.location=jQuery(this).find('a').attr('href');
	});

	jQuery('.text-overlay-parallax-mobile').each(function() {
		var height = jQuery(this).height();
		var photoHeight = jQuery(this).closest('.parallax-mobile-widget-tile').find('img.parallax-mobile-image').height();
		var new_padding = Math.floor((photoHeight - height)/2); 
		jQuery(this).css('padding-top', new_padding);
	});
	
});

jQuery(window).load(function() {

	$body = jQuery('.parallaxcontainer');
	if (jQuery('.parallaxcontainer').is('.instantiate-parallax')) {
		var s = skrollr.init({
			forceHeight: false
		}); 
		jQuery(".parallaxcontainer").css('height','auto');
		jQuery(".parallaxcontainer section").fadeIn(2000);
		s.refresh(jQuery('.homeSlide'));
	}
	
});

// make sure that all trending stories cards in a horizontal row have the same height for photo and text to create a nice, even display

jQuery(window).load(function() {
	
	jQuery('.sno-widget-card-wrapper.trending-horizontal').each(function() {
		console.log('yes')
		var height = 0; var tallest = 0; 
		
		jQuery(this).find('.sno-trending-card').each(function() {
			height = jQuery(this).height();
			tallest = (height > tallest) ? height : tallest;
		}); 
		console.log(tallest)
		jQuery(this).find('.sno-trending-card').height(tallest);
	});

});

// make sure that all trending stories cards are the same height

jQuery(window).load(function() {
	
	jQuery('.sno-widget-trending-wrapper.trending-horizontal').each(function() {
		
		var card_height = 0; 
		var tallest_card = 0;
		
		jQuery(this).find('.trending-row-wrap').each(function() {
			card_height = jQuery(this).height();
			tallest_card = (card_height > tallest_card) ? card_height : tallest_card;
		}); 
		jQuery(this).find('.trending-row-wrap').height(tallest_card);
	});

});

// make sure that all cards in a row have the same height for photo and text to create a nice, even display

jQuery(window).load(function() {
	
	jQuery('.sno-widget-card-wrapper.sno-card-row').each(function() {
		
		var title_height = 0; var teaser_height = 0;
		var tallest_title = 0; var tallest_teaser = 0;
		
		jQuery(this).find('.sno-content-card').each(function() {
			title_height = jQuery(this).find('.sno-profile-card-header-wrap').height();
			tallest_title = (title_height > tallest_title) ? title_height : tallest_title;
			teaser_height = jQuery(this).find('.sno-profile-card-teaser').height();
			tallest_teaser = (teaser_height > tallest_teaser) ? teaser_height : tallest_teaser;
		}); 
		jQuery(this).find('.sno-profile-card-header-wrap').height(tallest_title);
		jQuery(this).find('.sno-profile-card-teaser').height(tallest_teaser);
	});

});

// adjust the height of teasers so that text doesn't get cut off at the bottom of the widget

jQuery(document).ready(function() {
	jQuery('.sno-widget-card-wrapper.sno-card-row, .sno-widget-card-wrapper.sno-vertical-stack').each(function() {
		jQuery(this).find('.sno-content-card.sno-profile-small-photo').each(function() {
			var name_height = jQuery(this).find('.sno-profile-name').height();
			var title_height = jQuery(this).find('.sno-profile-title').height();
			var teaser_height = jQuery(this).find('p').height();
			var photo_height = jQuery(this).find('.sno-profile-card-image').height();
			var line_height = parseInt(jQuery(this).find('.sno-card-teaser p').css('line-height'));
			var font_size = parseInt(jQuery(this).find('.sno-card-teaser p').css('font-size'));
			var card_bottom_padding = parseInt(jQuery(this).css('padding-bottom'));
			
			var buffer = ( line_height - font_size ) / 2;
		
			if (photo_height != null) {
				var text_height_area_max = photo_height - name_height - title_height - 10 + card_bottom_padding;
				while (teaser_height >= text_height_area_max) teaser_height -= line_height;
				teaser_height += buffer;
			}
		});
		if (typeof(teaser_height) != "undefined" && teaser_height !== null) jQuery(this).find('.sno-profile-card-teaser').css({'height': teaser_height + 'px', 'overflow': 'hidden'});
	});
});

jQuery(window).load(function() {
	
	// sports score carousel widget
	
	jQuery(function($) {
		
		$('.sports-widget-carousel').each(function(){

			var direction = $(this).data('direction');
			var transition_speed = $(this).data('transition-speed');
			var slide_duration = $(this).data('slide-duration');
			var pause_button = $(this).data('pause-button');
			var margin_width = $(this).data('margin-width');
			var item_width = $(this).data('item-width');
			var move_number = $(this).data('move-number');
			
			if (direction == 'vertical') {
				item_width = null;
				margin_width = null;
			}
			
			
			var carousel = $(this);
			$(this).flexslider({
				animationSpeed: transition_speed,
				slideshowSpeed: slide_duration,
				slideshow: true,
				animation: 'slide',
				direction: direction,
				directionNav: false,
				animationLoop: true,
				controlNav: false,
				pausePlay: pause_button,
				minItems: 1,
				move: 1,
				maxItems: 5,
				itemWidth: item_width,
				itemMargin: margin_width,
				end : function(slider){
					console.log(carousel);
	                jQuery(carousel).find('.slides li').each(function(){
	                    slider.addSlide('<li>'+jQuery(this).context.innerHTML+'</li>', slider.count);
	                    jQuery(carousel).find('.slides').append('<li>'+jQuery(this).context.innerHTML+'</li>');
	                });
	            }				
			});
			
			$(this).find('.flex-viewport').css({'height': '200px!important'});
		});
	
	});
	
	// sports score hover effect for links
	
	jQuery('body').on('hover', 'a .scorewrap.sc_horizontal', function() {
		jQuery(this).toggleClass('sc_score_link');
	});
	
	jQuery('body').on('hover', 'a .scorewrap.sc_vertical', function() {
		jQuery(this).toggleClass('sc_score_link_vertical');
	});
	
	// schedule/results widgets -- show dropdown choice for individual sports
	jQuery('body').on('click', '.scheduleheader', function() {
		jQuery(this).closest('.widgetwrap').find('.widget-sports-list').slideToggle();
		jQuery(this).closest('.widgetwrap').find('.sno-sports-selector-icon').toggleClass('dashicons-arrow-down dashicons-arrow-up');
	});
	
	// make table cells clickble on sports tables
	jQuery('table.schedulewidget td').click(function() {
		var href = jQuery(this).find("a").attr("href");
		if (href) {
			window.location = href;
		}
	});

	// carousel widget -- instantiate flexslider for all carousel widgets
	
	jQuery(function($){
		
		$('.carousel-widget .carouselslider').each(function(){
			
			var transition_speed = $(this).data('transition-speed');
			var auto_scroll = $(this).data('auto-scroll');
			var transition_style = $(this).data('transition-style');
			var navigation_buttons = $(this).data('navigation-buttons');
			var width_adjustment = $(this).data('width-adjustment');
			var auto_scroll_speed = $(this).data('auto-scroll-speed');
			var display_number = $(this).data('display-number');
			var move_number = $(this).data('move-number');
			var margin_width = $(this).data('margin-width');
			var item_width = $(this).data('item-width');
			var show_thumbnails = $(this).data('show-thumbnails');
			var full_screen = $(this).data('full-screen');
			var thumbnail_width = $(this).data('thumbnail-width');
			var thumbnail_margin = $(this).data('thumbnail-margin');
			
			if (full_screen == 'widgetfullscreen' && display_number != '1') {
				item_width = (jQuery(window).width() - width_adjustment) / display_number;
			} else if (full_screen == 'widgetfullscreen') {
				item_width = jQuery(window).width();
			}

			if (show_thumbnails == 'on') {
				jQuery(this).closest('.carousel-widget').find('.thumbnailslider').flexslider({
					animation: 'slide',
					customDirectionNav: jQuery(this).closest('.carousel-widget').find('.thumbnailslider .custom-navigation span'), 
					controlNav: false,
					directionNav: true,
					animationLoop: true,
					slideshow: false,
					itemWidth: thumbnail_width,
					itemMargin: thumbnail_margin,
					touch: true,
					asNavFor: $(this).closest('.carousel-widget').find('.carouselslider'),
				});
			}
			
			$(this).flexslider({
				animationSpeed: transition_speed,
				animationLoop: true,
				customDirectionNav: jQuery(this).find('.custom-navigation span'), 
			    controlNav: navigation_buttons,
				smoothHeight: false,
				slideshowSpeed: auto_scroll_speed,
				slideshow: auto_scroll,
				animation: transition_style,
				sync: $(this).closest('.carousel-widget').find('.thumbnailslider'),
				itemWidth: item_width,
				itemMargin: margin_width,
				minItems: 1,
				move: move_number,
				maxItems: 5,
			});
			
			var height_method = $(this).data('height-method');
			var height_ratio = $(this).data('height-ratio');

			var carouselWidth = jQuery(this).width()
			var carouselHeight = Math.floor(jQuery(this).width() * height_ratio / 100);
			
			if (height_method == 'Ratio') jQuery(this).find('ul.slides li').css('height', carouselHeight);
		
		});

	});
	
});

function scalephoto() {
	jQuery(".slideshow-photo-container").each(function(){
		photo_w = jQuery(this).find('img').attr('data-width');
		photo_h = jQuery(this).find('img').attr('data-height');
		if (photo_h > 0) { photo_ratio = photo_w / photo_h; } else { photo_ratio = 2; }
		area_w = jQuery(this).width();
		area_h = jQuery(this).height();
		if (area_h > 0) { area_ratio = area_w / area_h; } else { area_ratio = 2; }
		photo_ratio = photo_ratio.toFixed(2); area_ratio = area_ratio.toFixed(2);
		ratio_diff = Math.abs(area_ratio - photo_ratio).toFixed(2);
		if (ratio_diff < .3 && photo_w > area_w * .7 && photo_h > area_h * .7) {
			jQuery(this).find('img').addClass('forcefill');
		} else {
			jQuery(this).find('img').removeClass('forcefill');
		}
	})
}

jQuery(function() {
	window.onresize = function() {
		scalephoto();
		scalephotos();
    };
});

function scalephotos() {
	jQuery("#sfi-slideshow ul.slides li.storyslide").each(function(){
		photo_w = jQuery(this).find('img').attr('data-width');
		photo_h = jQuery(this).find('img').attr('data-height');
		if (photo_h > 0) { photo_ratio = photo_w / photo_h; } else { photo_ratio = 2; }
		area_w = jQuery(this).find('.sfi-photo-wrap').width();
		area_h = jQuery(this).find('.sfi-photo-wrap').height();
		if (area_h > 0) { area_ratio = area_w / area_h; } else { area_ratio = 2; }
		photo_ratio = photo_ratio.toFixed(2); area_ratio = area_ratio.toFixed(2);
		ratio_diff = Math.abs(area_ratio - photo_ratio).toFixed(2);
		if (ratio_diff < .3 && photo_w > area_w * .7 && photo_h > area_h * .7) {
			jQuery(this).find('img').addClass('forcefill');
		} else {
			jQuery(this).find('img').removeClass('forcefill');
		}
	})
}

jQuery(document).ready(function() {
	
	// functions for closing remodal windows on scroll down
	
	var sno_slideshow_open = 'no';
	jQuery('body').keypress(function(e){
		if(e.which == 27 || e.which == 0){
			sno_slideshow_open = 'no';
		}
	});									

	jQuery('html').on('wheel', function(event) {
		var delta = {
			y: event.originalEvent.deltaY
		};
					
	if (delta.y > 20 && sno_slideshow_open == 'yes') {
			jQuery('button.remodal-close').trigger('click');
			sno_slideshow_open = 'no';
		}
	});
	
	// for images set as the featured image
	
	jQuery('.photooverlay, .photo-enlarge').click(function(){

		var photo = jQuery(this).data('photo-id');
		var story = jQuery(this).data('story-id');
		jQuery.ajax({
			url: frontend_ajax_object.ajaxurl,
			type:'POST',
			data: {
				action: 'snoloadimage',
				photo: photo,
				story: story
			},
			success:function(results) {	
				jQuery('.remodal-inner-container').replaceWith(results);
				sno_slideshow_open = 'yes';
				var inst = jQuery("[data-remodal-id=modal-photo]").remodal();
				inst.open();
				scalephoto();
				
			}
		});
	
	});


	// for images inserted into the body of the story
	
	jQuery('body').on('click', '.storycontent a', function(e){
		if (jQuery(this).find('img').length) {
			
			// test if the image is linked to something on current site vs. being linked to external site.  If linked externally, return true

			if (link_is_external(this)) {
   				return true;	
			} 
			
			function link_is_external(link_element) {
				return (link_element.host !== window.location.host);
			}
			
			var img_link = jQuery(this).attr('href');
			
			if (	img_link.indexOf(".jpg") === -1 && 
					img_link.indexOf(".jpeg") === -1 && 
					img_link.indexOf(".png") === -1 && 
					img_link.indexOf("attachment") === -1 
			) return true;
			
			var image_id = jQuery(this).find('img').attr('class').match(/[\w-]*wp-image-[\w-]*/g).toString().replace("wp-image-","");
			var post_id = jQuery(this).closest('div.snopostid').attr('id').toString().replace("snopostid-","");	
			
			jQuery.ajax({
               	url: frontend_ajax_object.ajaxurl,
				type: 'POST',
				data: {
					'action': 'snoloadimage',
					'photo': image_id,
					'story': post_id
				},
				success: function(results) {
					jQuery('.remodal-inner-container').replaceWith(results);
					sno_slideshow_open = 'yes';
					var inst = jQuery("[data-remodal-id=modal-photo]").remodal();
					inst.open();
					scalephoto();
				}
			});
    		
			return false;
		}		
	});

	// for SNO slideshows
	
	jQuery(".sfiphotowrap .slideshow-enlarge").click(function(){
		jQuery(this).closest(".sfiphotowrap").trigger('click');
	});

	jQuery('body').on('click', '.sfiphotowrap', function() {
		
		var image = jQuery(this).attr('data-photo-id');
		var storyid = jQuery(this).attr('data-story-id');
		var widget = jQuery(this).attr('data-widget');
		var photo_ids = jQuery(this).attr('data-photo-ids');
		var clicked_image = jQuery(this).find('li.flex-active-slide .inline-photo-wrap').data('image');
		if ( clicked_image == undefined ) var clicked_image = image;
		
						
		sno_slideshow_open = 'yes';
		jQuery('.remodal-inner-container').empty();
		var inst = jQuery("[data-remodal-id=modal-photo]").remodal();
		inst.open();
		
		jQuery.ajax({
			url: frontend_ajax_object.ajaxurl,
			type: 'POST',
			data: {
				'action': 'getslideshow',
				'storyid': storyid,
				'image': clicked_image,
				'photoids': photo_ids,
				'widget': widget
			},	
			success:function(results) { 
								
				jQuery('.remodal-inner-container').replaceWith(results);
				
				var start = jQuery('.remodal-inner-container').find('.slideshowdata').data('start');
				
				jQuery('.flexslider').animate({'opacity': 1}, { 'duration': 'slow'});
				jQuery('.flex-container').css('background', 'unset');

				jQuery('#sfi-thumbnails').flexslider({
					animation: 'slide',
					controlNav: false,
					customDirectionNav: jQuery("#sfi-thumbnav span"),
					animationLoop: true,
					slideshow: false,
					itemWidth: 106,
					itemMargin: 5,
					touch: true,
					asNavFor: '#sfi-slideshow'
				});

				jQuery('#sfi-slideshow').flexslider({
					animation: 'fade',
					smoothHeight: false,
					customDirectionNav: jQuery("#sfi-slideshow span"),
					animationLoop: true,
					slideshow: false,
					startAt: start,
					touch: true,
					sync: "#sfi-thumbnails"
				});
			
				var thumbAreaWidth = jQuery('#sfi-thumbnails').width();
				var thumbRowWidth = 111 * jQuery('#sfi-thumbnails li').length;
				if (thumbRowWidth < thumbAreaWidth) {
					jQuery('.sfi-thumbnails').width(thumbRowWidth);
				}
			
			
			}
			
		});
	
	});
	
	jQuery('body').on('click', 'div[data-remodal-action="close"]', function() {
		jQuery('button.remodal-close').trigger('click');
		sno_slideshow_open = 'no';
	});

	jQuery('body').on('click', 'button.sno-overlay-close', function() {
		sno_slideshow_open = 'no';
	});

  	// for inline slideshows in stories and widgets	
  	
	jQuery(function($){
		
		$('.inline-slideshow-area').each(function(){
			
			var smooth_height = $(this).find('.flex-container').data('smooth-height');
			var autoscroll_speed = $(this). find('.flex-container').data('autoscroll-speed');
			var autoscroll = $(this). find('.flex-container').data('autoscroll');
						
			$(this).find('.inline-thumbnails').flexslider({
				animation: 'slide',
				controlNav: false,
				customDirectionNav: $(this).closest('.inline-slideshow-area').find('.inline-thumbnav span'),
				animationLoop: true,
				slideshow: false,
				itemWidth: 107,
				itemMargin: 1,
				touch: true,
				asNavFor: $(this).closest('.inline-slideshow-area').find('.inline-slideshow')
			});
		
			$(this).find('.inline-slideshow').flexslider({
				animation: 'slide',
				smoothHeight: smooth_height,  
				controlNav: false,
				slideshowSpeed: autoscroll_speed, 
				slideshow: autoscroll,
				customDirectionNav: $(this).closest('.inline-slideshow-area').find('.inline-slideshow span'),
				animationLoop: true,
				touch: true,
				sync: $(this).closest('.inline-slideshow-area').find('.inline-thumbnails')
			});
			
			$(this).find('.custom-navigation').on('click', function() {
				return false;
			});

			var thumbAreaWidth = $(this).find('.inline-thumbnails').width();
			var thumbRowWidth = 111 * $(this).find('.inline-thumbnails li').length;
			if (thumbRowWidth < thumbAreaWidth) {
				$(this).find('.inline-thumbnails').width(thumbRowWidth);
			}

		});
		
	});	  		
	
	// activating modal window when sharing button clicked	
	
	jQuery('.modal-share').click(function() {
		if (jQuery(this).hasClass('share-email')) {
			var action = 'shareemail';
		} else {
			var action = 'sharestory';
		}
		var inst = jQuery('[data-remodal-id=modal-share]').remodal();
		inst.open();
	
		var sharestoryid = jQuery(this).closest('.sharing').data('story-id');
			
		jQuery.ajax({
			url: frontend_ajax_object.ajaxurl,
			type: 'POST',
			data: {
				'action': action,
				'sharestoryid': sharestoryid
			},
			success:function(results) { 
				jQuery(".remodal-share-inner-container").replaceWith(results); 
			}
		});
	
	});
	
	// when readers share stories via email

	jQuery(function($){
		
		$('body').on('click', '#submit_email', function(){
			
			var storyid = $(this).closest('.emailstoryform').data('story-id');
			var humanity = $(this).closest('.emailstoryform').data('humanity');
			var form_data = $(this).closest('form').serializeArray();
						
			$.ajax({
            	url:"/wp-admin/admin-ajax.php",
				type:'POST',
				data: {
					'action': 'emailstory',
					'storyid': storyid,
					'humanity': humanity,
					'form_data': form_data
				},
	            success: function(results) { 
		            $(".remodal-share-inner-container").replaceWith(results); 
				}
    	  	});

		});

	});
										
					

	// staff page links

	jQuery('tr.staffstoryrow').click(function() {
		var href = jQuery(this).find("a").attr("href");
		if(href) {
			window.location = href;
		}
	});
	
	jQuery('.clickable-row').click(function() {
		window.document.location = jQuery(this).data('href');
	});
	
	
	// grid widget -- hover and photo effects
	
	jQuery('.enlarge-effect').mouseenter(function() {
		jQuery(this).find('img').removeClass('shrink');
		jQuery(this).find('img').removeClass('grow');
		jQuery(this).find('img').addClass('grow');
	});
	jQuery('.enlarge-effect').mouseleave(function() {
		jQuery(this).find('img').addClass('shrink');
	});

	jQuery('.overlay-hover').mouseenter(function() {
		jQuery(this).find('.gridwidgetoverlay').fadeIn();
	});
	jQuery('.overlay-hover').mouseleave(function() {
		jQuery(this).find('.gridwidgetoverlay').fadeOut();
	})
		
	jQuery('.keep-link').click(function() {
		window.location=jQuery(this).find('a').attr('href');
	});

	
	// carousel widget effects

	jQuery('.carousel-cover-hover').mouseenter(function() {
		jQuery(this).find('.carouseloverlay').fadeIn();
		jQuery(this).find('.carouseloverlaytext').fadeIn();
	});
	jQuery('.carousel-cover-hover').mouseleave(function() {
		jQuery(this).find('.carouseloverlay').fadeOut();
		jQuery(this).find('.carouseloverlaytext').fadeOut();
	})

	jQuery('.carouseltext .continue').click(function() {
		window.location=jQuery(this).parent().find('a').attr('href');
	});
	
	jQuery('.carousel-cover-hover').click(function() {
		window.location=jQuery(this).find('.widgetheadlineoverlay a').attr('href');
	});

	
	// category widget
	jQuery('img.enlarge-effect').mouseenter(function() {
		jQuery(this).removeClass('shrink');
		jQuery(this).removeClass('grow');
		jQuery(this).addClass('grow');
	});
	jQuery('.enlarge-effect').mouseleave(function() {
		jQuery(this).addClass('shrink');
	});

	jQuery('.fw1-textarea .continue').click(function() {
		window.location=jQuery(this).closest('.fw1-panel').find('.widgetheadline a').attr('href');
	});

	jQuery('.fw2-textarea .continue').click(function() {
		window.location=jQuery(this).closest('.fw2-panel').find('.widgetheadline a').attr('href');
	});

	jQuery('.fw3-textarea .continue').click(function() {
		window.location=jQuery(this).closest('.fw3-panel').find('.widgetheadline a').attr('href');
	});

	jQuery('.wa-textarea .continue').click(function() {
		window.location=jQuery(this).closest('.wa-textarea').find('a.homeheadline').attr('href');
	});
	
	jQuery('.catwidget-col2 .continue').click(function() {
		window.location=jQuery(this).closest('.catwidget-col2').find('a.homeheadline').attr('href');
	});
	
		
	// go to top button on long form pages

	jQuery('#gototop').click(function () {
		var bottomPosition = jQuery(".phototop").height();
		var bottomWindow = jQuery(window).height();
		bottomWindow = bottomWindow - 50;
		if (bottomWindow < bottomPosition) {
			photoHeight = bottomWindow;
			photoHeight = photoHeight -45;
		} else {
			photoHeight = bottomPosition +35;
		}
		jQuery('html, body').animate({
			scrollTop: jQuery("#mainbody").offset().top-photoHeight
     	}, 1000).delay();
		return false;
	});

	// side menus on long form pages

	jQuery("#hover-menu").click(function(){
	    jQuery("#altheader-searchbox").toggle('slow');					
	});  

	if (jQuery('.slidemenu').is(":visible")) {
		jQuery('.hidethis').css({ visibility: "hidden" });
		jQuery('#altheader-searchbox').css({ zIndex: "1001"});
	} else {
		jQuery('.hidethis').css({ visibility: "visible" });
		jQuery('#altheader-searchbox').css({ zIndex: "99"});
	}

	jQuery('.sno-menu').click(function() {
	
		jQuery('#hoverbar_menu').fadeToggle();
		jQuery('.menu-icon').toggle();
		jQuery('.close-icon').toggle();
		jQuery('#hoverbar_menu').css({ height: jQuery(window).height() - 50 });
	
	});
	
	// comments box and links on story pages
	
	jQuery("#commentsbox").click(function() {
    	jQuery("#commentsbody").slideToggle('slow');
    	jQuery(".commenttoggle").toggleClass("fa-plus-square fa-minus-square");
    	if (jQuery(".commenttoggle").hasClass("fa-plus-square")) {
			jQuery(".commenttoggle").attr("aria-expanded", "false");
		} else {
			jQuery(".commenttoggle").attr("aria-expanded", "true");
	    }
	});
	
	jQuery("#commentslink, .commentscroll").click(function() {
    	jQuery("#commentsbody").slideDown('slow');
    	jQuery(".commenttoggle").addClass("fa-minus-square").removeClass("fa-plus-square").attr("aria-expanded", "true");
	});

	jQuery(function($) {
		$('.commentscroll').click(function() {
			var adjustment = 70; // set up a top margin for the scrolled-to element
			if ($(".navbarwrap").hasClass('sno-sticky')) {
				adjustment += jQuery('.navbarwrap').height();
			}
			if ($(".subnavbarwrap").hasClass('sno-sticky')) {
				adjustment += jQuery('.subnavbarwrap').height();
			}
			$('html, body').animate({ scrollTop: $("#commentswrap").offset().top - adjustment }, 500);
			return false;
		});
	});
	
	// WordPress automatically adjusts widths on left and right floated photos with captions.  It's annoying.  Let's readjust their adjustment. 

	jQuery(document).ready(function(){
		// jQuery(".wp-caption").removeAttr('style');
		jQuery.each(jQuery(".wp-caption"), function(){
			jQuery(this).width(jQuery(this).find('img').attr('width'));
		});
	})
	
	// activate search button on focus within the search box
	
	jQuery(function(){
		jQuery(".s").focus(function(){
			jQuery(".sno-submit-search-button").prop("disabled", false);
		});				
	});
	
	// Accessibility -- Set tabIndex to -1 so that top_level_links can't receive focus until menu is open
	
    var top_level_links = jQuery(".sf-menu").find('> li > a');
    jQuery(top_level_links).next('ul')
        .find('a')
            .attr('tabIndex',-1);

    // Accessibility -- Adding aria-haspopup for appropriate items
    
    jQuery(top_level_links).each(function(){
        if(jQuery(this).next('ul').length > 0)
            jQuery(this).parent('li').attr('aria-haspopup', 'true');
    });
    
    // Side menu on mobile view
   
 	jQuery( '#hover-menu-side' ).on( 'touchstart click', function(e) {
 		e.preventDefault();
 			jQuery('#sno_mobile_menu').fadeToggle();
		    jQuery("body").toggleClass('noscroll');

 	});
 	
 	jQuery( '.side-close-icon' ).on( 'touchstart click', function(e) {
 		e.preventDefault();
 			jQuery('#sno_mobile_menu').fadeToggle();
			    jQuery("body").toggleClass('noscroll');
 	});
 		

 	// Slideshows and photos need to open in remodal overlays -- prevent them from going to a new URL or jumping to anchor
 	
	jQuery("a[href='#slideshow']").on('click', function(event) { return false; });
	jQuery("a[href='#photo']").on('click', function(event) { return false; });

	// Create photo slide up effect as the reader scrolls down the page
	
	var win = jQuery(window);
	var allMods = jQuery(".sno-animate-active .sno-animate");
	var allWPcaptions = jQuery(".sno-animate-active .wp-caption");

	allMods.each(function(i, el) {
		var el = jQuery(el);
		if (el.visible(true)) {
			el.addClass("already-visible"); 
		} else {
			el.css('visibility', 'hidden');
		}
	});
	allWPcaptions.each(function(i, el) {
		var el = jQuery(el);
		if (el.visible(true)) {
			el.addClass("already-visible"); 
		} 
	});

	jQuery(window).scroll(function(event) {
		jQuery(".sno-animate-active .sno-animate").each(function(i, el) {
			var el = jQuery(el);
			if (el.visible(true)) {
				el.addClass("come-in");
				el.css('visibility', 'visible');
			}
		});
		jQuery(".sno-animate-active .wp-caption").each(function(i, el) {
			var el = jQuery(el);
			if (el.visible(true)) {
				el.addClass("come-in");
				el.css('visibility', 'visible');
			} 
		});
	});
	
	// create fade in loading effect for carousels

	jQuery(window).load(function() {
		jQuery('.flexslider').animate({'opacity': 1}, { 'duration': 'slow'});
		jQuery('.flex-container').css('background', 'unset');
	});

	// jump navigation options when Above Header widget area is activated
	
	if (jQuery('#upperwrap-outer').is(':visible')) {
		
		var top_elements = jQuery('#upperwrap-outer').offset().top;
		var wp_adminbar = 0;
		if (jQuery('#wpadminbar').length > 0) wp_adminbar = jQuery('#wpadminbar').height();
		
		var jumpbutton = top_elements + 20;
		var headerLocation   = jQuery("#wrap").offset().top;
		if ( headerLocation > jQuery(window).height() ) { 
			jQuery('#jump-to-header').css('top',jumpbutton + 'px').fadeIn();
		}
		jQuery(window).scroll(function () {
			if (jQuery(this).scrollTop() > (headerLocation - jQuery(window).height()) ) {
				jQuery('#jump-to-header').fadeOut();
				jQuery('#jump-arrow').fadeOut();
			} else {
				jQuery('#jump-to-header').fadeIn();
				jQuery('#jump-arrow').fadeIn();
			}
		});
	
		jQuery('#jump-to-header').click(function () {
			jQuery('html, body').animate({ scrollTop: jQuery("#wrap").offset().top - wp_adminbar }, 500);
			return false;
		});
		jQuery('#jump-arrow').click(function () {
			jQuery('html, body').animate({ scrollTop: jQuery("#wrap").offset().top - wp_adminbar }, 500);
			return false;
		});
	
	}

	// scroll to top functionality on long-form template

	jQuery("#snotop").hide();
	jQuery(function () {
		jQuery(window).scroll(function () {
			if (jQuery(this).scrollTop() > 400) {
				jQuery('#snotop').fadeIn();
			} else {
				jQuery('#snotop').fadeOut();
			}
			if (jQuery(this).scrollTop() > 200) {
				jQuery('.header').fadeIn();
			}
			
		});

		jQuery('#back-top a').click(function () {
			jQuery('body,html').animate({
				scrollTop: 0
			}, 800);
			return false;
		});

	});

	// immersive image on long-form -- jump to main story

	jQuery(document).ready(function() {
		if (jQuery('#mainbody').is(':visible')) {
			
			var wp_adminbar = 0;
			if (jQuery('#wpadminbar').length > 0) wp_adminbar = jQuery('#wpadminbar').height();
			
			jQuery('#jump-arrow').click(function () {
				jQuery('html, body').animate({ scrollTop: jQuery("#mainbody").offset().top - wp_adminbar }, 500);
				jQuery('#jump-arrow').fadeOut();
				return false;
			});

			jQuery(window).scroll(function () {
				if (jQuery(this).scrollTop() > jQuery(window).height() ) {
					jQuery('#jump-arrow').fadeOut();
				} else {
					jQuery('#jump-arrow').fadeIn();
				}
			});
			
		};
	});

});

// adjusting Top Menu if client has too many elements in it

jQuery(function($){

	if ($('.navbarwrap').length) {
	
		// passing this info in via data tags because the dom isn't fully loaded yet and measuring might not work consistently
		var navAlign = $('.navbarwrap').data('align');
		var navMargin = $('.navbarwrap').data('margin');
		var navStyle = $('.navbarwrap').data('style');
		var navMinilogo = $('.navbarwrap').data('minilogo');
		var navWidth = $('.navbarwrap').data('width');
		var navMode = $('.navbarwrap').data('mode');
		var navHeight = $('.navbarwrap').data('height');
		
		if ( navAlign != 'center' ) $('#navbar').css({"overflow-y": "hidden", "max-height": navHeight });
		
		$(window).load(function() {
			if ($( window).width() < 980) return; 
			if ( navAlign == 'center' ) {
				$('#navbar').css({"overflow-y": "unset", "max-height": "none", "height": "auto" });
				$('.navbarwrap, .navbarcontainer, #navbarbackground').css({"height": "fit-content" });
				return;
			}
			resizeMenuTop();
		})
		
		if ( navAlign == 'right' ) $('#menu-more-top').insertBefore('#menu-a-menu');
						
		function resizeMenuTop() {
			
			if ($(window).width() < 980) return;
	
			var moreWidthTop = 0; 
			var menubufferTop = 0; 
			var minilogoTop = 0;
	
			if ( $('#menu-more-top').is(':visible') ) moreWidthTop = $('#menu-more-top').width();
			
			if ( navStyle == 'Blocks' ) menubufferTop = navMargin;
			
			if ( navMinilogo == 'true' ) minilogoTop = jQuery('#mini-logo-top').width();
			
			if ( navMode == 'default' ) {
				var MenuTopWidth = navWidth;
			} else {
				var MenuTopWidth = $('#navbarbackground').width() - 10;
			}
	
			var browserWidth = $(window).width();
			if (browserWidth < MenuTopWidth) MenuTopWidth = browserWidth;
				
			if ($('#menu-a-menu').width() + menubufferTop + minilogoTop + moreWidthTop >= MenuTopWidth) { 
				
				$('.sno-top-menu > li:last-child').addClass('sub-menu'); 
				$('.sno-top-menu > li:last-child').appendTo('#add-more-top'); 
				if ($('#menu-more-top').is(':hidden')) { $('#menu-more-top').css({display: "block"}); }			
				if ($('#menu-a-menu').width() + menubufferTop + minilogoTop + moreWidthTop > MenuTopWidth) {
					resizeMenuTop(); 
				} else {
					$('#navbar').css("overflow-y", "unset");
					resizeMenuTop(); 
				}
		
			} else {
		
				$('#navbar').css("overflow-y", "unset");								
		
			}
			 
		}

	}
		
	$( window ).resize(function() { 
		if ( navAlign == 'center' ) return;
   		if ($(window).width() > 980 && $('.navbarwrap').length) resizeMenuTop();
	});   
	            		
});

// Adjusting bottom menu if client has too many items in it

jQuery(function($){
	
	if ($('.subnavbarwrap').length) {
	
		// passing this info in via data tags because the dom isn't fully loaded yet and measuring might not work consistently
		var navAlign = $('.subnavbarwrap').data('align');
		var navMargin = $('.subnavbarwrap').data('margin');
		var navStyle = $('.subnavbarwrap').data('style');
		var navMinilogo = $('.subnavbarwrap').data('minilogo');
		var navWidth = $('.subnavbarwrap').data('width');
		var navMode = $('.subnavbarwrap').data('mode');
		var navHeight = $('.subnavbarwrap').data('height');
			
		if ( navAlign != 'center' ) $('#subnavbar').css({"overflow-y": "hidden", 'max-height': navHeight });
		
		$(window).load(function() {
			if ( $( window).width() < 980 ) return; 
			if ( navAlign == 'center' ) {
				$('#subnavbar').css({"overflow-y": "unset", "max-height": "none", "height": "auto" });
				$('.subnavbarwrap, .subnavbarcontainer, #subnavbarbackground').css({"height": "fit-content" });
				return;
			}
			resizeMenuBottom();
		})
		
		if ( navAlign == 'right' ) $('#menu-more-bottom').insertBefore('#menu-b-menu');
						
		function resizeMenuBottom() {
			
			if ($(window).width() < 980) return;
	
			var moreWidthBottom = 0; 
			var menubufferBottom = 0; 
			var minilogoBottom = 0;
	
			if ( $('#menu-more-bottom').is(':visible') ) moreWidthBottom = $('#menu-more-bottom').width();
			
			if ( navStyle == 'Blocks' ) menubufferBottom = navMargin;
			
			if ( navMinilogo == 'true' ) minilogoBottom = jQuery('#mini-logo-bottom').width();
			
			if ( navMode == 'default' ) {
				var MenuBottomWidth = navWidth;
			} else {
				var MenuBottomWidth = $('#subnavbarbackground').width() - 10;
			}
	
			var browserWidth = $(window).width();
			if (browserWidth < MenuBottomWidth) MenuBottomWidth = browserWidth;
				
			if ($('#menu-b-menu').width() + menubufferBottom + minilogoBottom + moreWidthBottom >= MenuBottomWidth) { 
				
				$('.sno-bottom-menu > li:last-child').addClass('sub-menu'); 
				$('.sno-bottom-menu > li:last-child').appendTo('#add-more-bottom'); 
				if ($('#menu-more-bottom').is(':hidden')) { $('#menu-more-bottom').css({display: "block"}); }			
				if ($('#menu-b-menu').width() + menubufferBottom + minilogoBottom + moreWidthBottom > MenuBottomWidth) {
					resizeMenuBottom(); 
				} else {
					$('#subnavbar').css("overflow-y", "unset");
					resizeMenuBottom(); 
				}
			
			} else {
			
				$('#subnavbar').css("overflow-y", "unset");								
			
			}
		
		}
	}
	
	$( window ).resize(function() { 
		if ( navAlign == 'center' ) return;
   		if ($(window).width() > 980 && $('.subnavbarwrap').length) resizeMenuBottom();
	});   

});

jQuery(document).ready(function(){
	
	// moving on to other ideas.  Let's hide poll widgets if there are no polls
	jQuery("div:contains('there are no polls available at the moment')").closest('.widgetwrap').hide();
	
});

// jump navigation on long-form menu

jQuery(document).ready(function () {
	jQuery('li.longform-menu').click(function () {
		var target = jQuery(this).data('part');
		jQuery('html, body').animate({ scrollTop: jQuery("#spacer" + target).offset().top }, 500);
		return false;
	});
});

jQuery(function($) {
	$('body').on('click', '.moreheadlines', function(){
		var videoid = $(this).data('video');
		console.log(videoid)
		console.log('ahsdfas')
		$("#loadingimage").show();
		$("#videowrap").fadeOut();
		$.ajax({
			url: frontend_ajax_object.ajaxurl,
			type: 'POST',
			data: {
				'action': 'replace_video',
				'type': 'video',
				'id': videoid,
			},
			success:function(results) { 
				$("#moreposts").replaceWith(results); 
				$("#loadingimage").hide();
			}
		});
	});
});


jQuery(document).ready(function () {
	jQuery('body').on('click', '.sno-content-card', function() {
		var href = jQuery(this).find("a").attr("href");
		if(href) {
			window.location = href;
		}
	});
});