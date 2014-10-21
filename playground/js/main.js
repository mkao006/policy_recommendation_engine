require.config({

    baseUrl: 'js/libs',

    paths: {
        bootstrap       :   '//netdna.bootstrapcdn.com/bootstrap/3.0.1/js/bootstrap.min',
        backbone        :   '//cdnjs.cloudflare.com/ajax/libs/backbone.js/1.1.2/backbone-min',
        chosen          :   '//fenixapps.fao.org/repository/js/chosen/1.0.0/chosen.jquery.min',
        highcharts      :   '//code.highcharts.com/highcharts',
        jquery          :   '//code.jquery.com/jquery-1.10.1.min',
        mustache        :   '//cdnjs.cloudflare.com/ajax/libs/mustache.js/0.8.1/mustache',
        underscore      :   '//cdnjs.cloudflare.com/ajax/libs/underscore.js/1.6.0/underscore-min',
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
            console.debug(lang);
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