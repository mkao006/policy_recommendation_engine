require.config({

    baseUrl: 'js/libs',

    paths: {
        pre_core        :   '../pre-core/pre-core',
        pre_engine      :   '../pre-engine/pre-engine'
    },

    shim: {
        bootstrap: ['jquery'],
        backbone: {
            deps: ['jquery', 'underscore'],
            exports: 'Backbone'
        },
        chosen: ['jquery'],
        highcharts: ['jquery'],
        underscore: {
            exports: '_'
        }
    }

});

require(['jquery',
         'mustache',
         'backbone',
         'bootstrap',
         'domReady!'], function($, Mustache, Backbone) {

    /* Define the router. */
    var AppRouter = Backbone.Router.extend({

        /* Define the routes. */
        routes: {
            ''                  :   'home',
            '(/)home(/)'        :   'home',
            '(/)home(/):lang'   :   'home',
            '(/)engine(/)'      :   'engine',
            '(/)engine(/):lang' :   'engine'
        },

        /* Overwrite language settings. */
        init_language: function (lang) {
            lang = (lang != null) ? lang : 'en';
            require.config({'locale': lang});
        }

    });

    /* Initiate router. */
    var app_router = new AppRouter;

    /* Define routes endpoints: home. */
    app_router.on('route:home', function (lang) {
        this.init_language(lang);
        require(['pre_core'], function(PRE_CORE) {
            PRE_CORE.init({
                lang: lang,
                placeholder_id: 'placeholder'
            });
        });
    });

    /* Define routes endpoints: engine. */
    app_router.on('route:engine', function (lang) {
        this.init_language(lang);
        require(['pre_core'], function(PRE_CORE) {
            PRE_CORE.init({
                lang: lang,
                placeholder_id: 'placeholder'
            });
            require(['pre_engine'], function(PRE_ENGINE) {
                PRE_ENGINE.init({
                    lang: lang,
                    placeholder_id: 'main_content'
                });
            });
        });
    });

    /* Initiate Backbone history. */
    Backbone.history.start();

});