import * as const from 'const';
import 'services/validators';
import SomeService from 'services/SomeService';
import { observable, action } from 'mobx';
import { required, length, isPhone } from 'services/validators';
import { formatPhone } from 'services/utils';

export const api = 'url';

export default class State {}
