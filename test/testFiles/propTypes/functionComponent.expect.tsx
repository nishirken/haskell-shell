import React from 'react';

interface MyComponentProps {
  optionalString?: string;
  requiredNumber: number;
}

export const MyComponent: React.FunctionComponent<MyComponentProps> = () => (
  <div></div>
);

