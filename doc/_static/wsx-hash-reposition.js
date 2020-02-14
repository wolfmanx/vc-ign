// The issue of bootstrap navbar hiding an anchor for a location.hash
// (URL fragment) is discussed here:
//
// `Navbar hides initial content when jumping to in-page anchor · Issue #1768 · twbs/bootstrap`_
// .. _`Navbar hides initial content when jumping to in-page anchor · Issue #1768 · twbs/bootstrap`: https://github.com/twbs/bootstrap/issues/1768
//
// sphinx-bootstrap.js already contains the fix for that, which works
// fine with chrome, however, with firefox it does not work for some
// pages.

(function ($) {
    $(document).ready(function() {
        if(window.location.hash) {
            var location_hash = window.location.hash;
            var $html = $('html').first();

            var scroll_top = 0;
            var last_scroll_top = 0;

            var debounce_limit = 5;
            var debounced = 0;
            var debounce_next_action;;

            var debug = function () {
                // console.log.apply(this, arguments);
            };

            var adjust_scroll_top_deactivate_hash = function () {
                scroll_top = $html.scrollTop();
                debug('adjust_scroll_top_deactivate_hash st:', scroll_top, 'lst:', last_scroll_top);
                window.location.hash = '';
                adjust_scroll_top_debounce_start(adjust_scroll_top_activate_hash, 1);
            };

            var adjust_scroll_top_activate_hash = function () {
                scroll_top = $html.scrollTop();
                debug('adjust_scroll_top_activate_hash st:', scroll_top, 'lst:', last_scroll_top);
                if (!window.location.hash) {
                    window.location.hash = location_hash;
                }
            };

            var adjust_scroll_top_debounce = function () {
                scroll_top = $html.scrollTop();
                debug('adjust_scroll_top_debounce st:', scroll_top, 'lst:', last_scroll_top, 'deb:', debounced);
                if (last_scroll_top != scroll_top) {
                    last_scroll_top = scroll_top;
                    debounced = 0;
                    setTimeout(adjust_scroll_top_debounce, 100);
                } else {
                    debounced += 1;
                    if (debounced < debounce_limit) {
                        setTimeout(adjust_scroll_top_debounce, 100);
                    } else {
                        debounce_next_action();
                    }
                }
            }

            var adjust_scroll_top_debounce_start = function (action, limit, delay) {
                debounce_next_action = action;
                debounce_limit = limit || 5;
                debounced = 0;
                setTimeout(adjust_scroll_top_debounce, delay || 1);
            }

            debug('on_load st:', scroll_top, 'lst:', last_scroll_top);
            adjust_scroll_top_debounce_start(adjust_scroll_top_deactivate_hash, 5, 500);
        }
    });
}(window.$jqTheme || window.jQuery));
