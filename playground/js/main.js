require.config({

    baseUrl: 'js/libs',

    paths: {
        pre_core        :   '../pre-core/pre-core'
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
            '(/)home(/):lang'   :   'home'
        },

        /* Overwrite language settings. */
        init_language: function (lang) {
            lang = (lang != null) ? lang : 'en';
            require.config({'locale': lang});
        }

    });

    /* Initiate router. */
    var app_router = new AppRouter;

    /* Define routes endpoints. */
    app_router.on('route:home', function (lang) {
        this.init_language(lang);
        require(['pre_core'], function(PRE_CORE) {
            PRE_CORE.init({lang: lang});
        });
    });

    /* Initiate Backbone history. */
    Backbone.history.start();

});