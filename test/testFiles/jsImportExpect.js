import React, { Component } from 'react';
import PropTypes from 'prop-types';
import classNames from 'classnames';
import IconButton from 'material-ui/IconButton';
import ChevronDown from 'react-material-icon-svg/dist/ChevronDownIcon';
import ChevronUp from 'react-material-icon-svg/dist/ChevronUpIcon';
import colors from 'styles/variables/colors';

import classes from './Harmonica.scss';

export default class Harmonica extends Component {
  static propTypes = {
    opened: PropTypes.bool,
    children: PropTypes.node,
    onClick: PropTypes.func,
    colorize: PropTypes.bool,
    left: PropTypes.node,
    right: PropTypes.node,
    size: PropTypes.oneOf(['lg', 'md', 'sm']),
    disabled: PropTypes.bool,
    rootClassName: PropTypes.string,
    contentClassName: PropTypes.string,
  };

  static defaultProps = {
    size: 'sm',
  };

  onClick = () => {
    const { disabled, onClick } = this.props;
    !disabled && onClick();
  };

  render() {
    const {
      children,
      opened,
      left,
      right,
      colorize,
      size,
      disabled,
      rootClassName,
      contentClassName,
    } = this.props;
    const iconColor = colorize ? colors.white : colors.colorPrimary;
    const icon = opened ? (
      <ChevronUp fill={iconColor} />
    ) : (
      <ChevronDown fill={iconColor} />
    );

    const classNameHead = classNames(
      classes.head,
      disabled && classes.disabled,
      {
        [classes['colorize']]: colorize,
        [classes['size_sm']]: size === 'sm',
        [classes['size_md']]: size === 'md',
        [classes['size_lg']]: size === 'lg',
      }
    );

    const classNameRoot = classNames(classes.root, rootClassName);
    const classNameContent = classNames(classes.content, contentClassName);

    return (
      <div className={classNameRoot}>
        <div className={classNameHead} onClick={this.onClick}>
          <div className={classes.left}>{left}</div>
          <div className={classes.right}>
            {right}
            {!disabled && <IconButton>{icon}</IconButton>}
          </div>
        </div>
        {opened && <div className={classNameContent}>{children}</div>}
      </div>
    );
  }
}
