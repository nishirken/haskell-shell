import React from 'react';
import PropTypes from 'prop-types';

export const MyComponent = props => (
  <div></div>
);

MyComponent.propTypes = {
  optionalString: PropTypes.string,
  requiredNumber: PropTypes.number.isRequired,
};
