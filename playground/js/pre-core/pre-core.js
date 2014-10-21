define(['jquery',
        'mustache',
        'text!../pre-core/html/templates.html',
        'i18n!../pre-core/nls/translate',
        'bootstrap'], function ($, Mustache, templates, translate) {

    'use strict';

    function PRE_CORE() {

        this.CONFIG = {
            lang: 'en',
            'placeholder_id': 'placeholder'
        };

    }

    PRE_CORE.prototype.init = function(config) {

        /* Extend default configuration. */
        this.CONFIG = $.extend(true, {}, this.CONFIG, config);

        /* Render the main structure. */
        var template = $(templates).filter('#main_structure').html();
        var view = {
            title: translate.pre,
            subtitle: translate.pre_description
        };
        var render = Mustache.render(template, view);
        $('#' + this.CONFIG.placeholder_id).html(render);

    };

    return new PRE_CORE();

});