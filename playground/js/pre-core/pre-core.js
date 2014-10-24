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

    /**
     * This is the entry method to configure the module.
     *
     * @param config Custom configuration in JSON format to extend the default settings.
     */
    PRE_CORE.prototype.init = function(config) {

        /* Extend default configuration. */
        this.CONFIG = $.extend(true, {}, this.CONFIG, config);

        /* Render the main structure. */
        var template = $(templates).filter('#main_structure').html();
        var view = {
            title: translate.pre,
            subtitle: translate.pre_description,
            toggle_navigation: translate.toggle_navigation,
            engine: translate.engine,
            engine_link: '#/engine/' + this.CONFIG.lang,
            papers: translate.papers,
            papers_link: '#/paper/' + this.CONFIG.lang,
            competition: translate.competition,
            competition_link: '#/competition/' + this.CONFIG.lang,
            signin: translate.signin
        };
        var render = Mustache.render(template, view);
        $('#' + this.CONFIG.placeholder_id).html(render);

    };

    return new PRE_CORE();

});