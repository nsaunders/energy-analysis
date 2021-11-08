var suncalc = require("suncalc");

exports._getTimes = function(date) {
  return function(lat) {
    return function(lon) {
      return suncalc.getTimes(date, lat, lon);
    };
  };
};

exports._sunrise = function(times) {
  return times.sunrise;
};

exports._sunset = function(times) {
  return times.sunset;
};
