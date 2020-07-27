import { Elm } from './src/Main.elm';
import SupPort from '@ursi/support';

const app = Elm.Main.init({
	flags: {
		cards: JSON.parse(localStorage.cards || `[]`),
		max: +localStorage.max || 0,
	}
});

const port = SupPort(app.ports);

port(`ports`, {
	WriteMax(max) {
		localStorage.max = max;
	},
}, {}, true);

port(`card`, {
	Write(cards) {
		localStorage.cards = JSON.stringify(cards);
	},
}, {}, true);
