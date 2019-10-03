import React from 'react';
import PropTypes from 'prop-types';
import State from './State';
import {
  styles
} from './styles';

export class ClassComponent extends React.Component<any, any> {
  field1: any;
  field2: string = '';

  static propTypes = {
    // Можно объявить проп на соответствие определённому JS-типу.
    // По умолчанию это не обязательно.
    optionalArray: PropTypes.array,
    optionalBool: PropTypes.bool,
    optionalFunc: PropTypes.func,
    optionalNumber: PropTypes.number,
    optionalObject: PropTypes.object,
    optionalString: PropTypes.string,
    optionalSymbol: PropTypes.symbol,
  
    // Все, что может быть отрендерено:
    // числа, строки, элементы или массивы
    // (или фрагменты) содержащие эти типы
    optionalNode: PropTypes.node,
  
    // React-элемент
    optionalElement: PropTypes.element,
  
    // Тип React-элемент (например, MyComponent).
    optionalElementType: PropTypes.elementType,
    
    // Можно указать, что проп должен быть экземпляром класса
    // Для этого используется оператор `instanceof`.
    optionalMessage: PropTypes.instanceOf(Message),
  
    // Вы можете задать ограничение конкретными значениями
    // при помощи перечисления
    optionalEnum: PropTypes.oneOf(['News', 'Photos']),
  
    // Объект, одного из нескольких типов
    optionalUnion: PropTypes.oneOfType([
      PropTypes.string,
      PropTypes.number,
      PropTypes.instanceOf(Message)
    ]),
  
    // Массив объектов конкретного типа
    optionalArrayOf: PropTypes.arrayOf(PropTypes.number),
  
    // Объект со свойствами конкретного типа
    optionalObjectOf: PropTypes.objectOf(PropTypes.number),
  
    // Объект с определённой структурой
    optionalObjectWithShape: PropTypes.shape({
      color: PropTypes.string,
      fontSize: PropTypes.number
    }),
    
    // Объект со строгой структурой,
    // при наличии необъявленных свойств будут сформированы предупреждения
    optionalObjectWithStrictShape: PropTypes.exact({
      name: PropTypes.string.isRequired,
      quantity: PropTypes.number
    }),
  
    // Можно добавить`isRequired` к любому из приведённому выше типу,
    // чтобы показывать предупреждение,
    // если проп не передан
    requiredFunc: PropTypes.func.isRequired,
  
    // Значение любого типа
    requiredAny: PropTypes.any.isRequired,
  };

  private readonly state: State = new State();

  render() {
    return null;
  }
}
