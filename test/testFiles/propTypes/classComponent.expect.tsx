import React from 'react';

import State from './State';

interface ClassComponentProps {
  optionalArray?: any[];
  optionalBool?: boolean;
  optionalFunc?: (...any[]) => any;
  optionalNumber?: number;
  optionalObject?: object;
  optionalString?: string;
  optionalSymbol?: Symbol;
  optionalNode?: React.ReactNode;
  optionalElement?: React.ReactElement<any>;
  optionalElementType?: React.ComponentType<any>;
  optionalMessage?: Message;
  optionalEnum?: 'News' | 'Photos'
  optionalUnion?: string | number | Message;
  optionalArrayOf?: number[];
  optionalObjectOf?: Record<string | number, number>;
  optionalObjectWithShape: {
    color?: string;
    fontSize?: number;
  };
  optionalObjectWithStrictShape: {
    name: string;
    quantity?: number;
  };
  requiredFunc: (...any[]) => any;
  requiredAny: any;
}

export class ClassComponent extends React.Component<ClassComponentProps> {

  private readonly state: State = new State();

  render() {
    return null;
  }
}
