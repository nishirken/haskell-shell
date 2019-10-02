import React from 'react';
import {
  styles
} from './styles';

export interface MyComponentProps {
  optionalString?: string;
  requiredNumber: number;
}

export const MyComponent: React.FunctionalComponent<MyComponentProps> = props => (
  <div></div>
);

