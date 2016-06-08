# Competition: From Fog Nets to Neural Nets

This is the code to my submission for the competition 'From Fog Nets to Neural Nets' hosted by [Driven Data](https://www.drivendata.org). One particular challenge of the competition were missing values (not at random) and the temporal aspect of the data.

### Description
The competition was provided by Dar Si Hmad (DSH) and the 
> challenge is to develop a model that will predict the yield of DSH’s fog nets for every day during an evaluation period, using historical data about meteorological conditions and the fog net installations.

Meteorological data was available from sites directly adjacent to the location of the fog nets. Since these had a substantial amount of missing values, additional data about meteorological conditions at three sites far away from the fog nets were given in addition.

### Approach
Due to the complexity of the problem I used multiple steps to generate my submission. These were
 * Utilise additional meteorological data to impute missing values where possible
 * Build blocks of continuous (temporal) data chunks
 * Use time series modelling and Kalman filtering to impute the remaining missing values in the meteorological data
 * Build a hierarchical model to account for the abundend zeros in the water yield
    * 1st Level: Random forest to predict zero yield events
    * 2nd Level: Generalised additive model


Updated description coming soon.
