import React from 'react';

export interface MyComponentProps {
  optionalString?: string;
  requiredNumber: number;
}

export const MyComponent: React.FunctionalComponent<MyComponentProps> = props => (
  <div></div>
);

